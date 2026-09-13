(** Name classification.

    Fig. 2.3 has separate productions for a term variable [x], a fn item [f], a
    struct [S], a const [C] and a variant [E::V]; Fig. 2.2 has separate
    productions for a type parameter [T], an ADT [S<..>] and a transparent alias
    [A<..>].  The token stream does not say which of them a given name is; the
    program's own declarations plus the prelude of Fig. 2.7 do.  This pass reads
    them and replaces every [UNRESOLVED] constructor of {!Ast}.

    It is *not* Ch. 3's resolution: it settles the sort of a name, nothing else.
    The three shorthand-path forms and the bare prelude variant stay as the sugar
    constructors [GRAMMAR.md] gives them, for the spec's $\desug$ to eliminate.

    {1 The procedure}

    Ordered tests, first match wins.  The declaration sets are the program's own
    (every crate of the file, collected by {!collect}) plus Fig. 2.7's prelude
    and foreign declarations, listed literally below.  [GRAMMAR.md] §7 repeats
    the procedure, because the spec side needs it to read the converter's output.

    - A type name [TName (n, ts, rs)]:
      + [n] is a type parameter in scope and takes no arguments -> [TPARAM n];
      + [n] is a transparent alias -> [ALIASU];
      + otherwise -> [ADT].

      A parameter is in scope when the enclosing item's [gparam]s bind it, plus
      [Self] inside a trait or impl header ("trait and impl headers bind it in
      Gamma", §2.2).  The five primitive spellings ([bool], [i32], [u8],
      [usize], [str]) never reach here: {!Parse} maps an unapplied one straight
      to its own constructor.
    - A bare name in path position ([PName], and [EIdent] for a name written
      with no turbofish): prelude variant -> [PBVARIANT]; fn item -> [PFN];
      const item -> [PCONST]; struct -> [PSTRUCT]; otherwise the
      {e capitalisation fallback}: an initial upper-case letter -> [PSTRUCT],
      anything else -> [PFN].  [EIdent] is an [EVAR] when none of the four sets
      contains the name, which is the common case for a term variable.
    - A two-segment name [PName2 (a, b)] (module segments are already dropped by
      {!Parse}): [Box::new] / [Box::leak] -> the prelude fn items [box_new] /
      [box_leak] (§2.7, in prose); [a] an enum -> [PVARIANT]; [a] a trait ->
      [PDSHORT]; otherwise -> [PSHORT] over [a] read as a type.
    - A qualified path [PQual (t, D<..>, x)]: [PQCONST] when [D] declares [x] as
      a [const], [PQFN] otherwise.

    {1 Two limits, recorded}

    The capitalisation fallback is a guess, reached only for a name that no
    declaration of the program and no entry of the prelude introduces.  Such a
    program is ill formed anyway -- Ch. 11's collection has nothing to bind the
    name to -- so the guess decides only which error a later chapter reports.

    [EIdent] has {e no local-binding scope}: the sets are top-level declarations,
    so a [let], a closure parameter or a fn parameter that shadows a declared fn,
    const, struct or prelude-variant name would still resolve to the path rather
    than to [EVAR].  No program of the acceptance corpus does that; the one
    near-miss, [weird], is a fn item in witness 96460 and a parameter in witness
    141713, which are different files and so different declaration sets.  Fixing
    it means threading the binders of [ELET], [ECLOSURE] and [fparam] through
    this pass, and nothing needs it yet. *)

open Ast
module S = Set.Make (String)
module M = Map.Make (String)

type kind = KFn | KConst | KType

type ctx = {
  aliases : S.t;
  structs : S.t;
  enums : S.t;
  variants : string M.t;  (* prelude variant V |-> its enum E *)
  traits : S.t;
  trait_items : kind M.t M.t;
  fns : S.t;
  consts : S.t;
  tparams : S.t;
}

(* ------------------------------------------------------------------ *)
(* The prelude and the foreign declarations (Fig. 2.7)                *)
(* ------------------------------------------------------------------ *)

let prelude_traits =
  [
    "Sized"; "Copy"; "Unsize"; "CoerceUnsized"; "Future"; "FnOnce"; "FnMut";
    "Fn"; "Try"; "FromResidual"; "From"; "AsRef"; "Default"; "Iterator";
    "Display"; "Any"; "Unpin"; "PartialEq";
  ]

let prelude_trait_items =
  [
    ("Future", [ ("Output", KType) ]);
    ("FnOnce", [ ("Output", KType) ]);
    ("Try", [ ("Output", KType); ("Residual", KType); ("branch", KFn) ]);
    ("FromResidual", [ ("from_residual", KFn) ]);
    ("From", [ ("from", KFn) ]);
    ("AsRef", [ ("as_ref", KFn) ]);
    ("Default", [ ("default", KFn) ]);
    ("Iterator", [ ("Item", KType) ]);
    ("PartialEq", [ ("eq", KFn) ]);
  ]

let prelude_structs = [ "Box"; "PhantomData"; "String" ]

(* The three prelude enums are the only ones whose variants Rust brings into
   scope unqualified (§2.9, "prelude variants"). *)
let prelude_variants =
  [
    ("None", "Option"); ("Some", "Option"); ("Ok", "Result"); ("Err", "Result");
    ("Break", "ControlFlow"); ("Continue", "ControlFlow");
  ]

let prelude_enums = [ "Option"; "Result"; "ControlFlow" ]
let prelude_fns = [ "box_new"; "box_leak" ]

let empty_ctx =
  {
    aliases = S.empty;
    structs = S.of_list prelude_structs;
    enums = S.of_list prelude_enums;
    variants = M.of_seq (List.to_seq prelude_variants);
    traits = S.of_list prelude_traits;
    trait_items =
      M.of_seq
        (List.to_seq
           (List.map
              (fun (d, its) -> (d, M.of_seq (List.to_seq its)))
              prelude_trait_items));
    fns = S.of_list prelude_fns;
    consts = S.empty;
    tparams = S.empty;
  }

(* ------------------------------------------------------------------ *)
(* Collecting the program's own declarations                          *)
(* ------------------------------------------------------------------ *)

let kind_of_titem = function
  | TIType (a, _, _) -> (a, KType)
  | TIConst (n, _) -> (n, KConst)
  | TIFn (f, _, _, _, _) -> (f, KFn)

let collect_item ctx = function
  | IStruct (_, s, _, _, _) | ITupStruct (_, s, _, _, _) ->
      { ctx with structs = S.add s ctx.structs }
  | IEnum (_, e, _, _, _) -> { ctx with enums = S.add e ctx.enums }
  | ITrait (_, d, _, _, _, its) ->
      {
        ctx with
        traits = S.add d ctx.traits;
        trait_items =
          M.add d (M.of_seq (List.to_seq (List.map kind_of_titem its)))
            ctx.trait_items;
      }
  | IFn (_, _, f, _, _, _, _, _) -> { ctx with fns = S.add f ctx.fns }
  | IConst (_, n, _, _) -> { ctx with consts = S.add n ctx.consts }
  | IAlias (_, a, _, _) -> { ctx with aliases = S.add a ctx.aliases }
  | IImpl _ -> ctx

let collect (PGM crates) =
  List.fold_left
    (fun ctx (CRATE (_, items)) -> List.fold_left collect_item ctx items)
    empty_ctx crates

(* ------------------------------------------------------------------ *)
(* Scopes                                                             *)
(* ------------------------------------------------------------------ *)

let tparam_of = function
  | GTy t | GTyB (t, _) -> Some t
  | GRg _ | GRgB _ -> None

let with_gparams ctx gs =
  {
    ctx with
    tparams = List.fold_left (fun s g ->
        match tparam_of g with Some t -> S.add t s | None -> s) ctx.tparams gs;
  }

(* "trait and impl headers bind [Self] in Γ" (§2.2). *)
let with_self ctx = { ctx with tparams = S.add "Self" ctx.tparams }

(* ------------------------------------------------------------------ *)
(* Rewriting                                                          *)
(* ------------------------------------------------------------------ *)

let rec typ ctx = function
  | TName (n, [], []) when S.mem n ctx.tparams -> TParam n
  | TName (n, ts, rs) when S.mem n ctx.aliases ->
      AliasU (n, List.map (typ ctx) ts, rs)
  | TName (n, ts, rs) -> Adt (n, List.map (typ ctx) ts, rs)
  | Ref (r, t) -> Ref (r, typ ctx t)
  | RefMut (r, t) -> RefMut (r, typ ctx t)
  | FnPtr (bs, ts, t) -> FnPtr (bs, List.map (typ ctx) ts, typ ctx t)
  | Adt (n, ts, rs) -> Adt (n, List.map (typ ctx) ts, rs)
  | AliasU (n, ts, rs) -> AliasU (n, List.map (typ ctx) ts, rs)
  | Tup ts -> Tup (List.map (typ ctx) ts)
  | Arr (t, n) -> Arr (typ ctx t, n)
  | DynW (bs, r) -> DynW (List.map (bound ctx) bs, r)
  | Proj (t, tr, a) -> Proj (typ ctx t, traitref ctx tr, a)
  | ImplT (bs, c) -> ImplT (List.map (bound ctx) bs, c)
  | TShort (t, a) -> TShort (typ ctx t, a)
  | (Unit | BoolT | I32 | U8 | Usize | Str | TParam _ | Hole) as t -> t

and traitref ctx (TR (d, ts, rs)) = TR (d, List.map (typ ctx) ts, rs)

and bound ctx = function
  | BTrait (d, ts, rs, cs) ->
      BTrait (d, List.map (typ ctx) ts, rs, List.map (constr ctx) cs)
  | BForall (bs, b) -> BForall (bs, bound ctx b)
  | (BRegion _ | BRelax) as b -> b

and constr ctx = function
  | CEq (a, t) -> CEq (a, typ ctx t)
  | CBnd (a, bs) -> CBnd (a, List.map (bound ctx) bs)

let bassert ctx =
  let rec go = function
    | BATy (t, bs) -> BATy (typ ctx t, List.map (bound ctx) bs)
    | BARg (r, rs) -> BARg (r, rs)
    | BAForall (rs, b) -> BAForall (rs, go b)
  in
  go

(* A bare name in path position. *)
let path_of_name ctx n =
  if M.mem n ctx.variants then PBVariant n
  else if S.mem n ctx.fns then PFn n
  else if S.mem n ctx.consts then PConst n
  else if S.mem n ctx.structs then PStruct n
  else if n <> "" && n.[0] >= 'A' && n.[0] <= 'Z' then PStruct n
  else PFn n

let path ctx = function
  | PName n -> path_of_name ctx n
  | PName2 (a, b) ->
      (* "the witnesses' `Box::new` and `Box::leak` paths name exactly them"
         (§2.7): the two prelude fn items. *)
      if a = "Box" && (b = "new" || b = "leak") then PFn ("box_" ^ b)
      else if S.mem a ctx.enums then PVariant (a, b)
      else if S.mem a ctx.traits then PDShort (a, b)
      else PShort (typ ctx (TName (a, [], [])), b)
  | PQual (t, TR (d, ts, rs), x) ->
      let t = typ ctx t and tr = TR (d, List.map (typ ctx) ts, rs) in
      let k =
        match M.find_opt d ctx.trait_items with
        | Some items -> M.find_opt x items
        | None -> None
      in
      if k = Some KConst then PQConst (t, tr, x) else PQFn (t, tr, x)
  | PShort (t, x) -> PShort (typ ctx t, x)
  | PQFn (t, tr, x) -> PQFn (typ ctx t, traitref ctx tr, x)
  | PQConst (t, tr, x) -> PQConst (typ ctx t, traitref ctx tr, x)
  | (PFn _ | PStruct _ | PVariant _ | PConst _ | PBVariant _ | PDShort _) as p -> p

let targs ctx = function
  | TANone -> TANone
  | TASome (ts, rs) -> TASome (List.map (typ ctx) ts, rs)

let rec term ctx = function
  | EIdent n ->
      if
        M.mem n ctx.variants || S.mem n ctx.fns || S.mem n ctx.consts
        || S.mem n ctx.structs
      then EPath (path_of_name ctx n, TANone)
      else EVar n
  | EPath (p, ta) -> EPath (path ctx p, targs ctx ta)
  | ECall (e, es) -> ECall (term ctx e, List.map (term ctx) es)
  | ETup es -> ETup (List.map (term ctx) es)
  | EStruct (s, ta, fs) ->
      EStruct (s, targs ctx ta, List.map (fun (x, e) -> (x, term ctx e)) fs)
  | EArray es -> EArray (List.map (term ctx) es)
  | ERepeat (e, n) -> ERepeat (term ctx e, n)
  | ERef e -> ERef (term ctx e)
  | ERefMut e -> ERefMut (term ctx e)
  | EDeref e -> EDeref (term ctx e)
  | EField (e, x) -> EField (term ctx e, x)
  | EAssign (l, e) -> EAssign (lv ctx l, term ctx e)
  | ECast (e, t) -> ECast (term ctx e, typ ctx t)
  | ETry e -> ETry (term ctx e)
  | EClosure (m, ps, r, e) ->
      let ps = List.map (function
          | CaPat q -> CaPat q
          | CaPatT (q, t) -> CaPatT (q, typ ctx t)) ps in
      let r = match r with RtNone -> RtNone | RtSome t -> RtSome (typ ctx t) in
      EClosure (m, ps, r, term ctx e)
  | EAsync (m, e) -> EAsync (m, term ctx e)
  | EAwait e -> EAwait (term ctx e)
  | EMatch (e, arms) ->
      EMatch (term ctx e, List.map (fun (q, e) -> (q, term ctx e)) arms)
  | ELoop e -> ELoop (term ctx e)
  | EReturn e -> EReturn (term ctx e)
  | ELet (q, t, e1, e2) -> ELet (q, typ ctx t, term ctx e1, term ctx e2)
  | ESeq (e1, e2) -> ESeq (term ctx e1, term ctx e2)
  | (EVar _ | ELit _) as e -> e

and lv ctx = function
  | LvVar x -> LvVar x
  | LvDeref e -> LvDeref (term ctx e)
  | LvField (e, x) -> LvField (term ctx e, x)

let gparam ctx = function
  | GTyB (t, bs) -> GTyB (t, List.map (bound ctx) bs)
  | g -> g

let fparam ctx = function
  | APat (q, t) -> APat (q, typ ctx t)
  | ASelf t -> ASelf (typ ctx t)

let titem ctx = function
  | TIType (a, bs, w) ->
      TIType (a, List.map (bound ctx) bs, List.map (bassert ctx) w)
  | TIConst (n, t) -> TIConst (n, typ ctx t)
  | TIFn (f, gs, ps, ret, w) ->
      let ctx = with_gparams ctx gs in
      TIFn (f, List.map (gparam ctx) gs, List.map (fparam ctx) ps,
            typ ctx ret, List.map (bassert ctx) w)

let iitem ctx = function
  | IIType (a, t, w) -> IIType (a, typ ctx t, List.map (bassert ctx) w)
  | IIConst (n, t, e) -> IIConst (n, typ ctx t, term ctx e)
  | IIFn (f, gs, ps, ret, w, e) ->
      let ctx = with_gparams ctx gs in
      IIFn (f, List.map (gparam ctx) gs, List.map (fparam ctx) ps,
            typ ctx ret, List.map (bassert ctx) w, term ctx e)

let item ctx = function
  | IStruct (v, s, gs, w, fs) ->
      let ctx = with_gparams ctx gs in
      IStruct (v, s, List.map (gparam ctx) gs, List.map (bassert ctx) w,
               List.map (fun (SF (x, t)) -> SF (x, typ ctx t)) fs)
  | ITupStruct (v, s, gs, w, ts) ->
      let ctx = with_gparams ctx gs in
      ITupStruct (v, s, List.map (gparam ctx) gs, List.map (bassert ctx) w,
                  List.map (typ ctx) ts)
  | IEnum (v, e, gs, w, vs) ->
      let ctx = with_gparams ctx gs in
      IEnum (v, e, List.map (gparam ctx) gs, List.map (bassert ctx) w,
             List.map (fun (VAR (n, ts)) -> VAR (n, List.map (typ ctx) ts)) vs)
  | ITrait (v, d, gs, sup, w, its) ->
      let ctx = with_self (with_gparams ctx gs) in
      ITrait (v, d, List.map (gparam ctx) gs, List.map (bound ctx) sup,
              List.map (bassert ctx) w, List.map (titem ctx) its)
  | IImpl (v, gs, tr, self, w, its) ->
      let ctx = with_self (with_gparams ctx gs) in
      IImpl (v, List.map (gparam ctx) gs, traitref ctx tr, typ ctx self,
             List.map (bassert ctx) w, List.map (iitem ctx) its)
  | IFn (v, aq, f, gs, ps, ret, w, e) ->
      let ctx = with_gparams ctx gs in
      IFn (v, aq, f, List.map (gparam ctx) gs, List.map (fparam ctx) ps,
           typ ctx ret, List.map (bassert ctx) w, term ctx e)
  | IConst (v, n, t, e) -> IConst (v, n, typ ctx t, term ctx e)
  | IAlias (v, a, gs, t) ->
      let ctx = with_gparams ctx gs in
      IAlias (v, a, List.map (gparam ctx) gs, typ ctx t)

let pgm (PGM crates as p) =
  let ctx = collect p in
  PGM (List.map (fun (CRATE (c, its)) -> CRATE (c, List.map (item ctx) its)) crates)
