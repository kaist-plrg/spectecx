(** Recursive-descent parser for the Rust subset of Figs. 2.1-2.3.

    The subset has no binary operators, so the only precedence to encode is
    prefix [&]/[*], postfix call/field/[?], and [as]; everything else is driven
    by a keyword.  Blocks are the one place where Rust's grammar is not LR: a
    block-expression statement ([loop {} e]) carries no [;].  The recursive
    descent decides it with one look at the following token, which is why this
    target has a [parse.ml] rather than a Menhir [parse.mly].

    The parser produces an {!Ast} that still contains the [UNRESOLVED]
    constructors; {!Resolve} turns them into the productions of [GRAMMAR.md]. *)

open Common.Source

(* ------------------------------------------------------------------ *)
(* Token cursor                                                       *)
(* ------------------------------------------------------------------ *)

module Cursor = struct
  type t = { mutable toks : (Lexer.token * region) list }

  let make toks = { toks }
  let peek c = match c.toks with [] -> Lexer.Eof | (t, _) :: _ -> t
  let nth c i =
    match List.nth_opt c.toks i with Some (t, _) -> t | None -> Lexer.Eof

  let peek2 c = nth c 1
  let region c = match c.toks with [] -> no_region | (_, at) :: _ -> at

  let advance c =
    match c.toks with
    | (Lexer.Eof, _) :: _ -> ()
    | _ :: rest -> c.toks <- rest
    | [] -> ()

  let fail c msg =
    Error.error (region c)
      (Printf.sprintf "%s, got %s" msg (Lexer.string_of_token (peek c)))

  let expect c t =
    if peek c = t then advance c
    else fail c (Printf.sprintf "expected %s" (Lexer.string_of_token t))

  let eat c t = if peek c = t then (advance c; true) else false

  let ident c =
    match peek c with
    | Lexer.Ident s -> advance c; s
    | _ -> fail c "expected an identifier"

  let lifetime c =
    match peek c with
    | Lexer.Lifetime s -> advance c; s
    | _ -> fail c "expected a lifetime"

  let num c =
    match peek c with
    | Lexer.Num (n, _) -> advance c; n
    | _ -> fail c "expected a number"
end

open Lexer
open Ast

let region_of_lifetime = function
  | "static" -> RStatic
  | "_" -> RAnon
  | s -> RName s

(* `c::m1::…::mj::X` drops its module segments (§2.9, "module segments").
   What is left is one name, or the two names of a shorthand path. *)
let rec segments c acc =
  let acc = Cursor.ident c :: acc in
  if Cursor.peek c = ColonColon && (match Cursor.peek2 c with Ident _ -> true | _ -> false)
  then (Cursor.advance c; segments c acc)
  else List.rev acc

let drop_modules (segs : string list) : string list =
  let is_module s = s <> "" && s.[0] >= 'a' && s.[0] <= 'z' in
  let rec go = function
    | s :: (_ :: _ as rest) when is_module s -> go rest
    | l -> l
  in
  go segs

let path_name c : string list =
  match drop_modules (segments c []) with
  | [] -> Cursor.fail c "expected a path"
  | l -> l

(* ------------------------------------------------------------------ *)
(* Types, bounds, regions                                             *)
(* ------------------------------------------------------------------ *)

let primitive = function
  | "bool" -> Some BoolT
  | "i32" -> Some I32
  | "u8" -> Some U8
  | "usize" -> Some Usize
  | "str" -> Some Str
  | _ -> None

let rec parse_typ c : typ =
  match Cursor.peek c with
  | LParen ->
      Cursor.advance c;
      if Cursor.eat c RParen then Unit
      else begin
        let t = parse_typ c in
        if Cursor.peek c = Comma then begin
          let ts = ref [ t ] in
          while Cursor.eat c Comma && Cursor.peek c <> RParen do
            ts := parse_typ c :: !ts
          done;
          Cursor.expect c RParen;
          Tup (List.rev !ts)
        end
        else (Cursor.expect c RParen; t)
      end
  | Amp ->
      Cursor.advance c;
      let r = parse_opt_region c in
      if Cursor.eat c KwMut then RefMut (r, parse_typ c) else Ref (r, parse_typ c)
  | LBrack ->
      Cursor.advance c;
      let t = parse_typ c in
      Cursor.expect c Semi;
      let n = Cursor.num c in
      Cursor.expect c RBrack;
      Arr (t, n)
  | Underscore -> Cursor.advance c; Hole
  | KwFn -> Cursor.advance c; parse_fnptr c []
  | KwFor ->
      (* `for<'a> fn(..) -> ..` : the fn-pointer binder row of §2.9. *)
      Cursor.advance c;
      let rs = parse_binder c in
      Cursor.expect c KwFn;
      parse_fnptr c rs
  | KwDyn ->
      Cursor.advance c;
      let bs = parse_bounds c in
      (* `dyn β̄ + 'r`: a trailing region bound is the dyn type's region.  A dyn
         written without one keeps `None` — Ch. 3's mark, which runs before
         desug, must tell it from one written `'_`. *)
      let rec split acc = function
        | [ BRegion r ] -> (List.rev acc, Some r)
        | [] -> (List.rev acc, None)
        | b :: bs -> split (b :: acc) bs
      in
      let bs, r = split [] bs in
      DynW (bs, r)
  | KwImpl ->
      Cursor.advance c;
      let bs, cap = parse_impl_bounds c in
      ImplT (bs, cap)
  | Lt ->
      Cursor.advance c;
      let self = parse_typ c in
      if Cursor.eat c KwAs then begin
        let tr = parse_traitref c in
        Cursor.expect c Gt;
        Cursor.expect c ColonColon;
        Proj (self, tr, Cursor.ident c)
      end
      else begin
        Cursor.expect c Gt;
        Cursor.expect c ColonColon;
        TShort (self, Cursor.ident c)
      end
  | Ident _ -> (
      match path_name c with
      | [ n ] ->
          let ts, rs = parse_opt_args c in
          if ts = [] && rs = [] then
            match primitive n with Some t -> t | None -> TName (n, [], [])
          else TName (n, ts, rs)
      | [ a; b ] ->
          (* `T::A` / `S::A`: the shorthand-path row of §2.9. *)
          TShort (TName (a, [], []), b)
      | _ -> Cursor.fail c "a path with more than two non-module segments")
  | _ -> Cursor.fail c "expected a type"

and parse_fnptr c rs =
  Cursor.expect c LParen;
  let ts = ref [] in
  while Cursor.peek c <> RParen do
    ts := parse_typ c :: !ts;
    if Cursor.peek c <> RParen then Cursor.expect c Comma
  done;
  Cursor.expect c RParen;
  (* elided returns (§2.9) *)
  let ret = if Cursor.eat c Arrow then parse_typ c else Unit in
  FnPtr (rs, List.rev !ts, ret)

and parse_binder c =
  Cursor.expect c Lt;
  let rs = ref [] in
  while Cursor.peek c <> Gt do
    rs := Cursor.lifetime c :: !rs;
    if Cursor.peek c <> Gt then Cursor.expect c Comma
  done;
  Cursor.expect c Gt;
  List.rev !rs

(* An elided region is `'_` (§2.9, "elided regions"). *)
and parse_opt_region c =
  match Cursor.peek c with
  | Lifetime s -> Cursor.advance c; region_of_lifetime s
  | _ -> RAnon

(* `<τ̄, r̄>` on a struct, enum, alias or trait-reference use. *)
and parse_opt_args c : typ list * region list =
  if Cursor.peek c <> Lt then ([], [])
  else begin
    Cursor.advance c;
    let ts = ref [] and rs = ref [] in
    while Cursor.peek c <> Gt do
      (match Cursor.peek c with
       | Lifetime s -> Cursor.advance c; rs := region_of_lifetime s :: !rs
       | _ -> ts := parse_typ c :: !ts);
      if Cursor.peek c <> Gt then Cursor.expect c Comma
    done;
    Cursor.expect c Gt;
    (List.rev !ts, List.rev !rs)
  end

and parse_traitref c : traitref =
  match path_name c with
  | [ n ] ->
      let ts, rs = parse_opt_args c in
      TR (n, ts, rs)
  | _ -> Cursor.fail c "expected a trait reference"

(* β̄ : a `+`-separated bound list. *)
and parse_bounds c : bound list =
  let bs = ref [ parse_bound c ] in
  let rec loop () =
    if Cursor.peek c = Plus then begin
      Cursor.advance c;
      bs := parse_bound c :: !bs;
      loop ()
    end
  in
  loop ();
  List.rev !bs

(* `impl β̄ cap`: the capture list, when written, is the last `+ use<…>`. *)
and parse_impl_bounds c : bound list * cap =
  let bs = ref [] and cap = ref CapNone in
  let rec loop first =
    if first || Cursor.peek c = Plus then begin
      if not first then Cursor.advance c;
      if Cursor.peek c = KwUse then begin
        Cursor.advance c;
        Cursor.expect c Lt;
        let ts = ref [] and rs = ref [] in
        while Cursor.peek c <> Gt do
          (match Cursor.peek c with
           | Lifetime s -> Cursor.advance c; rs := s :: !rs
           | _ -> ts := Cursor.ident c :: !ts);
          if Cursor.peek c <> Gt then Cursor.expect c Comma
        done;
        Cursor.expect c Gt;
        cap := CapUse (List.rev !ts, List.rev !rs)
      end
      else begin
        bs := parse_bound c :: !bs;
        loop false
      end
    end
  in
  loop true;
  (List.rev !bs, !cap)

and parse_bound c : bound =
  match Cursor.peek c with
  | Question ->
      Cursor.advance c;
      let n = Cursor.ident c in
      if n <> "Sized" then Cursor.fail c "the only relaxation is `?Sized`";
      BRelax
  | Lifetime s -> Cursor.advance c; BRegion (region_of_lifetime s)
  | KwFor ->
      Cursor.advance c;
      let rs = parse_binder c in
      BForall (rs, parse_bound c)
  | Ident _ -> (
      match path_name c with
      | [ n ] ->
          if Cursor.peek c = LParen then parse_paren_fn c n
          else
            let ts, rs, cs = parse_bound_args c in
            BTrait (n, ts, rs, cs)
      | _ -> Cursor.fail c "expected a bound")
  | _ -> Cursor.fail c "expected a bound"

(* `D(τ̄) -> τ` and `D(τ̄)`: the parenthesized-Fn rows of §2.9. *)
and parse_paren_fn c n =
  Cursor.expect c LParen;
  let ts = ref [] in
  while Cursor.peek c <> RParen do
    ts := parse_typ c :: !ts;
    if Cursor.peek c <> RParen then Cursor.expect c Comma
  done;
  Cursor.expect c RParen;
  let arg =
    match List.rev !ts with [] -> Unit | [ t ] -> t | ts -> Tup ts
  in
  let ret = if Cursor.eat c Arrow then parse_typ c else Unit in
  BTrait (n, [ arg ], [], [ CEq ("Output", ret) ])

(* `<τ̄, r̄, δ̄>` on a bound: type, region and associated-item arguments mixed. *)
and parse_bound_args c : typ list * region list * constr list =
  if Cursor.peek c <> Lt then ([], [], [])
  else begin
    Cursor.advance c;
    let ts = ref [] and rs = ref [] and cs = ref [] in
    while Cursor.peek c <> Gt do
      (match (Cursor.peek c, Cursor.peek2 c) with
       | Lifetime s, _ -> Cursor.advance c; rs := region_of_lifetime s :: !rs
       | Ident a, Eq ->
           Cursor.advance c; Cursor.advance c;
           cs := CEq (a, parse_typ c) :: !cs
       | Ident a, Colon ->
           Cursor.advance c; Cursor.advance c;
           cs := CBnd (a, parse_bounds c) :: !cs
       | _ -> ts := parse_typ c :: !ts);
      if Cursor.peek c <> Gt then Cursor.expect c Comma
    done;
    Cursor.expect c Gt;
    (List.rev !ts, List.rev !rs, List.rev !cs)
  end

(* ------------------------------------------------------------------ *)
(* Patterns                                                           *)
(* ------------------------------------------------------------------ *)

let rec parse_pat c : pat =
  match Cursor.peek c with
  | Underscore -> Cursor.advance c; QWild
  | Ident _ -> (
      match (path_name c, Cursor.peek c) with
      | [ n ], LParen -> QTupStruct (n, parse_pats c)
      | [ a; b ], LParen -> QVariant (a, b, parse_pats c)
      | [ n ], _ -> QVar n
      | _ -> Cursor.fail c "expected a pattern")
  | _ -> Cursor.fail c "expected a pattern"

and parse_pats c =
  Cursor.expect c LParen;
  let qs = ref [] in
  while Cursor.peek c <> RParen do
    qs := parse_pat c :: !qs;
    if Cursor.peek c <> RParen then Cursor.expect c Comma
  done;
  Cursor.expect c RParen;
  List.rev !qs

(* ------------------------------------------------------------------ *)
(* Terms                                                              *)
(* ------------------------------------------------------------------ *)

let lv_of_term c = function
  | EIdent x | EVar x -> LvVar x
  | EDeref e -> LvDeref e
  | EField (e, x) -> LvField (e, x)
  | _ -> Cursor.fail c "the left side of an assignment is not a place"

let rec parse_term c : term = parse_assign c

and parse_assign c =
  let e = parse_cast c in
  if Cursor.peek c = Eq then begin
    let l = lv_of_term c e in
    Cursor.advance c;
    EAssign (l, parse_assign c)
  end
  else e

and parse_cast c =
  let e = ref (parse_prefix c) in
  while Cursor.eat c KwAs do
    e := ECast (!e, parse_typ c)
  done;
  !e

and parse_prefix c =
  match Cursor.peek c with
  | Amp ->
      Cursor.advance c;
      (* `&e` and `&mut e` take no region: Fig. 2.3 writes neither. *)
      if Cursor.eat c KwMut then ERefMut (parse_prefix c) else ERef (parse_prefix c)
  | Star -> Cursor.advance c; EDeref (parse_prefix c)
  | KwMove | Pipe | PipePipe -> parse_closure c
  | _ -> parse_postfix c

and parse_closure c =
  let mv = if Cursor.eat c KwMove then MvMove else MvNone in
  let ps =
    if Cursor.eat c PipePipe then []
    else begin
      Cursor.expect c Pipe;
      let ps = ref [] in
      while Cursor.peek c <> Pipe do
        let q = parse_pat c in
        let p = if Cursor.eat c Colon then CaPatT (q, parse_typ c) else CaPat q in
        ps := p :: !ps;
        if Cursor.peek c <> Pipe then Cursor.expect c Comma
      done;
      Cursor.expect c Pipe;
      List.rev !ps
    end
  in
  let ret = if Cursor.eat c Arrow then RtSome (parse_typ c) else RtNone in
  (* Rust needs a block when the closure declares a return type; when it does
     not, a block is still allowed and is what Unparse emits. *)
  let body =
    if Cursor.peek c = LBrace then begin
      Cursor.advance c;
      let e = parse_block c in
      Cursor.expect c RBrace;
      e
    end
    else parse_term c
  in
  EClosure (mv, ps, ret, body)

and parse_postfix c =
  let e = ref (parse_primary c) in
  let rec loop () =
    match Cursor.peek c with
    | LParen ->
        Cursor.advance c;
        let args = ref [] in
        while Cursor.peek c <> RParen do
          args := parse_term c :: !args;
          if Cursor.peek c <> RParen then Cursor.expect c Comma
        done;
        Cursor.expect c RParen;
        e := ECall (!e, List.rev !args);
        loop ()
    | Dot -> (
        Cursor.advance c;
        match Cursor.peek c with
        | KwAwait -> Cursor.advance c; e := EAwait !e; loop ()
        | Ident x -> Cursor.advance c; e := EField (!e, x); loop ()
        | Num (n, _) -> Cursor.advance c; e := EField (!e, Bigint.to_string n); loop ()
        | _ -> Cursor.fail c "expected a field name after `.`")
    | Question -> Cursor.advance c; e := ETry !e; loop ()
    | _ -> ()
  in
  loop ();
  !e

and parse_targs c : targs =
  if Cursor.peek c = ColonColon && Cursor.peek2 c = Lt then begin
    Cursor.advance c;
    Cursor.advance c;
    let ts = ref [] and rs = ref [] in
    while Cursor.peek c <> Gt do
      (match Cursor.peek c with
       | Lifetime s -> Cursor.advance c; rs := region_of_lifetime s :: !rs
       | _ -> ts := parse_typ c :: !ts);
      if Cursor.peek c <> Gt then Cursor.expect c Comma
    done;
    Cursor.expect c Gt;
    TASome (List.rev !ts, List.rev !rs)
  end
  else TANone

and parse_primary c =
  match Cursor.peek c with
  | Num (n, suf) ->
      Cursor.advance c;
      ELit
        (match suf with
         | None -> LNum n
         | Some "i32" -> LI32 n
         | Some "u8" -> LU8 n
         | Some "usize" -> LUsize n
         | Some s -> Cursor.fail c (Printf.sprintf "unknown literal suffix `%s`" s))
  | KwTrue -> Cursor.advance c; ELit LTrue
  | KwFalse -> Cursor.advance c; ELit LFalse
  | LParen ->
      Cursor.advance c;
      if Cursor.eat c RParen then ELit LUnit
      else begin
        let e = parse_term c in
        if Cursor.peek c = Comma then begin
          let es = ref [ e ] in
          while Cursor.eat c Comma && Cursor.peek c <> RParen do
            es := parse_term c :: !es
          done;
          Cursor.expect c RParen;
          ETup (List.rev !es)
        end
        else (Cursor.expect c RParen; e)
      end
  | LBrack ->
      Cursor.advance c;
      if Cursor.eat c RBrack then EArray []
      else begin
        let e = parse_term c in
        if Cursor.eat c Semi then begin
          let n = Cursor.num c in
          Cursor.expect c RBrack;
          ERepeat (e, n)
        end
        else begin
          let es = ref [ e ] in
          while Cursor.eat c Comma && Cursor.peek c <> RBrack do
            es := parse_term c :: !es
          done;
          Cursor.expect c RBrack;
          EArray (List.rev !es)
        end
      end
  | KwLoop ->
      Cursor.advance c;
      Cursor.expect c LBrace;
      let e = parse_block c in
      Cursor.expect c RBrace;
      ELoop e
  | KwReturn -> Cursor.advance c; EReturn (parse_term c)
  | KwAsync ->
      Cursor.advance c;
      let mv = if Cursor.eat c KwMove then MvMove else MvNone in
      Cursor.expect c LBrace;
      let e = parse_block c in
      Cursor.expect c RBrace;
      EAsync (mv, e)
  | KwMatch ->
      Cursor.advance c;
      let scrut = parse_term c in
      Cursor.expect c LBrace;
      let arms = ref [] in
      while Cursor.peek c <> RBrace do
        let q = parse_pat c in
        Cursor.expect c FatArrow;
        let e = parse_term c in
        arms := (q, e) :: !arms;
        ignore (Cursor.eat c Comma)
      done;
      Cursor.expect c RBrace;
      EMatch (scrut, List.rev !arms)
  | Lt ->
      Cursor.advance c;
      let self = parse_typ c in
      if Cursor.eat c KwAs then begin
        let tr = parse_traitref c in
        Cursor.expect c Gt;
        Cursor.expect c ColonColon;
        let x = Cursor.ident c in
        EPath (PQual (self, tr, x), parse_targs c)
      end
      else begin
        Cursor.expect c Gt;
        Cursor.expect c ColonColon;
        let x = Cursor.ident c in
        EPath (PShort (self, x), parse_targs c)
      end
  | Ident _ -> (
      let segs = path_name c in
      let ta = parse_targs c in
      let p =
        match segs with
        | [ n ] -> if ta = TANone then None else Some (PName n)
        | [ a; b ] -> Some (PName2 (a, b))
        | _ -> Cursor.fail c "a path with more than two non-module segments"
      in
      match p with
      | None -> (
          let n = List.hd segs in
          (* `S ta { x̄ : e }`: a struct literal. *)
          if Cursor.peek c = LBrace then parse_struct_lit c n ta else EIdent n)
      | Some p ->
          if Cursor.peek c = LBrace then
            match segs with
            | [ n ] -> parse_struct_lit c n ta
            | _ -> EPath (p, ta)
          else EPath (p, ta))
  | _ -> Cursor.fail c "expected an expression"

and parse_struct_lit c n ta =
  Cursor.expect c LBrace;
  let fs = ref [] in
  while Cursor.peek c <> RBrace do
    let x = Cursor.ident c in
    Cursor.expect c Colon;
    fs := (x, parse_term c) :: !fs;
    if Cursor.peek c <> RBrace then Cursor.expect c Comma
  done;
  Cursor.expect c RBrace;
  EStruct (n, ta, List.rev !fs)

(* A block body, between `{` and `}`.  Empty body and statement tail are the
   §2.9 rows of the same name. *)
and parse_block c : term =
  if Cursor.peek c = RBrace then ELit LUnit else parse_stmts c

and parse_stmts c : term =
  match Cursor.peek c with
  | KwLet ->
      Cursor.advance c;
      let q = parse_pat c in
      (* unannotated `let` (§2.9) gets a hole *)
      let t = if Cursor.eat c Colon then parse_typ c else Hole in
      Cursor.expect c Eq;
      let e = parse_term c in
      Cursor.expect c Semi;
      let rest = if Cursor.peek c = RBrace then ELit LUnit else parse_stmts c in
      ELet (q, t, e, rest)
  | _ ->
      let e = parse_term c in
      if Cursor.eat c Semi then
        if Cursor.peek c = RBrace then ESeq (e, ELit LUnit)
        else ESeq (e, parse_stmts c)
      else if Cursor.peek c = RBrace then e
      else ESeq (e, parse_stmts c)

(* ------------------------------------------------------------------ *)
(* Items                                                              *)
(* ------------------------------------------------------------------ *)

let parse_gparams c : gparam list =
  if Cursor.peek c <> Lt then []
  else begin
    Cursor.advance c;
    let gs = ref [] in
    while Cursor.peek c <> Gt do
      (match Cursor.peek c with
       | Lifetime s ->
           Cursor.advance c;
           if Cursor.eat c Colon then begin
             let rs = ref [ Cursor.lifetime c ] in
             while Cursor.eat c Plus do rs := Cursor.lifetime c :: !rs done;
             gs := GRgB (s, List.rev !rs) :: !gs
           end
           else gs := GRg s :: !gs
       | _ ->
           let n = Cursor.ident c in
           if Cursor.eat c Colon then gs := GTyB (n, parse_bounds c) :: !gs
           else gs := GTy n :: !gs);
      if Cursor.peek c <> Gt then Cursor.expect c Comma
    done;
    Cursor.expect c Gt;
    List.rev !gs
  end

let rec parse_bassert c : bassert =
  match Cursor.peek c with
  | KwFor ->
      Cursor.advance c;
      let rs = parse_binder c in
      BAForall (rs, parse_bassert c)
  | Lifetime s when Cursor.peek2 c = Colon ->
      Cursor.advance c;
      Cursor.advance c;
      let rs = ref [ region_of_lifetime (Cursor.lifetime c) ] in
      while Cursor.eat c Plus do
        rs := region_of_lifetime (Cursor.lifetime c) :: !rs
      done;
      BARg (region_of_lifetime s, List.rev !rs)
  | _ ->
      let t = parse_typ c in
      Cursor.expect c Colon;
      BATy (t, parse_bounds c)

let parse_where c : bassert list =
  if not (Cursor.eat c KwWhere) then []
  else begin
    let bs = ref [] in
    let rec loop () =
      match Cursor.peek c with
      | LBrace | Semi | Eof -> ()
      | _ ->
          bs := parse_bassert c :: !bs;
          if Cursor.eat c Comma then loop ()
    in
    loop ();
    List.rev !bs
  end

(* `& 'a? mut? self`: the receiver rows of §2.9. *)
let is_self_receiver c =
  let i = if (match Cursor.nth c 1 with Lifetime _ -> true | _ -> false) then 2 else 1 in
  let i = if Cursor.nth c i = KwMut then i + 1 else i in
  Cursor.nth c i = KwSelf

let parse_fparams c : fparam list =
  Cursor.expect c LParen;
  let ps = ref [] in
  while Cursor.peek c <> RParen do
    (match Cursor.peek c with
     | KwSelf ->
         Cursor.advance c;
         if Cursor.eat c Colon then ps := ASelf (parse_typ c) :: !ps
         else ps := ASelf (TName ("Self", [], [])) :: !ps
     | Amp when is_self_receiver c ->
         (* receivers `&'a self` and `&'a mut self` (§2.9) *)
         Cursor.advance c;
         let r = parse_opt_region c in
         let mut = Cursor.eat c KwMut in
         Cursor.expect c KwSelf;
         let self = TName ("Self", [], []) in
         ps := ASelf (if mut then RefMut (r, self) else Ref (r, self)) :: !ps
     | _ ->
         let q = parse_pat c in
         Cursor.expect c Colon;
         ps := APat (q, parse_typ c) :: !ps);
    if Cursor.peek c <> RParen then Cursor.expect c Comma
  done;
  Cursor.expect c RParen;
  List.rev !ps

(* elided returns (§2.9) *)
let parse_ret c = if Cursor.eat c Arrow then parse_typ c else Unit

let parse_body c =
  Cursor.expect c LBrace;
  let e = parse_block c in
  Cursor.expect c RBrace;
  e

let parse_titem c : titem =
  match Cursor.peek c with
  | KwType ->
      Cursor.advance c;
      let a = Cursor.ident c in
      let bs = if Cursor.eat c Colon then parse_bounds c else [] in
      let w = parse_where c in
      Cursor.expect c Semi;
      TIType (a, bs, w)
  | KwConst ->
      Cursor.advance c;
      let n = Cursor.ident c in
      Cursor.expect c Colon;
      let t = parse_typ c in
      Cursor.expect c Semi;
      TIConst (n, t)
  | KwFn ->
      Cursor.advance c;
      let f = Cursor.ident c in
      let gs = parse_gparams c in
      let ps = parse_fparams c in
      let ret = parse_ret c in
      let w = parse_where c in
      Cursor.expect c Semi;
      TIFn (f, gs, ps, ret, w)
  | _ -> Cursor.fail c "expected a trait item"

let parse_iitem c : iitem =
  match Cursor.peek c with
  | KwType ->
      Cursor.advance c;
      let a = Cursor.ident c in
      Cursor.expect c Eq;
      let t = parse_typ c in
      let w = parse_where c in
      Cursor.expect c Semi;
      IIType (a, t, w)
  | KwConst ->
      Cursor.advance c;
      let n = Cursor.ident c in
      Cursor.expect c Colon;
      let t = parse_typ c in
      Cursor.expect c Eq;
      let e = parse_term c in
      Cursor.expect c Semi;
      IIConst (n, t, e)
  | KwFn ->
      Cursor.advance c;
      let f = Cursor.ident c in
      let gs = parse_gparams c in
      let ps = parse_fparams c in
      let ret = parse_ret c in
      let w = parse_where c in
      IIFn (f, gs, ps, ret, w, parse_body c)
  | _ -> Cursor.fail c "expected an impl item"

let parse_item c : item =
  let vis = if Cursor.eat c KwPub then VPub else VPriv in
  match Cursor.peek c with
  | KwStruct ->
      Cursor.advance c;
      let s = Cursor.ident c in
      let gs = parse_gparams c in
      if Cursor.peek c = LParen then begin
        Cursor.advance c;
        let ts = ref [] in
        while Cursor.peek c <> RParen do
          ts := parse_typ c :: !ts;
          if Cursor.peek c <> RParen then Cursor.expect c Comma
        done;
        Cursor.expect c RParen;
        let w = parse_where c in
        Cursor.expect c Semi;
        ITupStruct (vis, s, gs, w, List.rev !ts)
      end
      else begin
        (* The where clause comes before both endings, so it is parsed before
           the `;`/`{` is looked at: `struct S<T> where T: D;` is as much a unit
           struct as `struct S;`. *)
        let w = parse_where c in
        if Cursor.eat c Semi then
          (* `struct S;` is the empty named-field list (§2.2) *)
          IStruct (vis, s, gs, w, [])
        else begin
        Cursor.expect c LBrace;
        let fs = ref [] in
        while Cursor.peek c <> RBrace do
          let x = Cursor.ident c in
          Cursor.expect c Colon;
          fs := SF (x, parse_typ c) :: !fs;
          if Cursor.peek c <> RBrace then Cursor.expect c Comma
        done;
        Cursor.expect c RBrace;
        IStruct (vis, s, gs, w, List.rev !fs)
        end
      end
  | KwEnum ->
      Cursor.advance c;
      let e = Cursor.ident c in
      let gs = parse_gparams c in
      let w = parse_where c in
      Cursor.expect c LBrace;
      let vs = ref [] in
      while Cursor.peek c <> RBrace do
        let v = Cursor.ident c in
        let ts = ref [] in
        if Cursor.eat c LParen then begin
          while Cursor.peek c <> RParen do
            ts := parse_typ c :: !ts;
            if Cursor.peek c <> RParen then Cursor.expect c Comma
          done;
          Cursor.expect c RParen
        end;
        vs := VAR (v, List.rev !ts) :: !vs;
        if Cursor.peek c <> RBrace then Cursor.expect c Comma
      done;
      Cursor.expect c RBrace;
      IEnum (vis, e, gs, w, List.rev !vs)
  | KwTrait ->
      Cursor.advance c;
      let d = Cursor.ident c in
      let gs = parse_gparams c in
      let sup = if Cursor.eat c Colon then parse_bounds c else [] in
      let w = parse_where c in
      Cursor.expect c LBrace;
      let its = ref [] in
      while Cursor.peek c <> RBrace do
        its := parse_titem c :: !its
      done;
      Cursor.expect c RBrace;
      ITrait (vis, d, gs, sup, w, List.rev !its)
  | KwImpl ->
      Cursor.advance c;
      let gs = parse_gparams c in
      let tr = parse_traitref c in
      Cursor.expect c KwFor;
      let self = parse_typ c in
      let w = parse_where c in
      Cursor.expect c LBrace;
      let its = ref [] in
      while Cursor.peek c <> RBrace do
        its := parse_iitem c :: !its
      done;
      Cursor.expect c RBrace;
      IImpl (vis, gs, tr, self, w, List.rev !its)
  | KwAsync | KwFn ->
      let aq = if Cursor.eat c KwAsync then AqAsync else AqNone in
      Cursor.expect c KwFn;
      let f = Cursor.ident c in
      let gs = parse_gparams c in
      let ps = parse_fparams c in
      let ret = parse_ret c in
      let w = parse_where c in
      IFn (vis, aq, f, gs, ps, ret, w, parse_body c)
  | KwConst ->
      Cursor.advance c;
      let n = Cursor.ident c in
      Cursor.expect c Colon;
      let t = parse_typ c in
      Cursor.expect c Eq;
      let e = parse_term c in
      Cursor.expect c Semi;
      IConst (vis, n, t, e)
  | KwType ->
      Cursor.advance c;
      let a = Cursor.ident c in
      let gs = parse_gparams c in
      Cursor.expect c Eq;
      let t = parse_typ c in
      Cursor.expect c Semi;
      IAlias (vis, a, gs, t)
  | _ -> Cursor.fail c "expected an item"

(* ------------------------------------------------------------------ *)
(* Crates                                                             *)
(* ------------------------------------------------------------------ *)

let parse_directive c (s : string) : crateid =
  let words = String.split_on_char ' ' s |> List.filter (fun w -> w <> "") in
  match words with
  | [ "crate"; name; loc; ed ] ->
      let cloc =
        match loc with
        | "local" -> Local
        | "foreign" -> Foreign
        | _ -> Cursor.fail c "a crate directive's locality is `local` or `foreign`"
      in
      let ced =
        match ed with
        | "2021" -> E2021
        | "2024" -> E2024
        | _ -> Cursor.fail c "a crate directive's edition is 2021 or 2024"
      in
      { cname = name; cloc; ced }
  | _ -> Cursor.fail c "expected `//@ crate <name> (local|foreign) (2021|2024)`"

let parse_items c =
  let its = ref [] in
  let rec loop () =
    match Cursor.peek c with
    | Eof | Directive _ -> ()
    | _ ->
        its := parse_item c :: !its;
        loop ()
  in
  loop ();
  List.rev !its

let parse_pgm c ~(default : crateid) : pgm =
  (* `::` evaluates right-to-left in OCaml, so each crate is bound before the
     tail is parsed. *)
  let rec loop first =
    match Cursor.peek c with
    | Eof -> []
    | Directive s ->
        Cursor.advance c;
        let cid = parse_directive c s in
        let items = parse_items c in
        CRATE (cid, items) :: loop false
    | _ when first ->
        let items = parse_items c in
        CRATE (default, items) :: loop false
    | _ -> Cursor.fail c "expected a crate directive or an item"
  in
  let crates = loop true in
  PGM crates

(* ------------------------------------------------------------------ *)
(* Entry points                                                       *)
(* ------------------------------------------------------------------ *)

let crate_name_of_file (filename : string) : string =
  Filename.remove_extension (Filename.basename filename)

let parse_ast ~(filename : string) ~(edition : edition) (source : string) : pgm =
  let c = Cursor.make (Lexer.tokenize ~filename source) in
  let default =
    { cname = crate_name_of_file filename; cloc = Local; ced = edition }
  in
  let pgm = parse_pgm c ~default in
  (match Cursor.peek c with
   | Eof -> ()
   | _ -> Cursor.fail c "unexpected trailing token");
  Resolve.pgm pgm
