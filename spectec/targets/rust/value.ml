(** {!Ast} <-> IL value of [pgm], exactly the table of [GRAMMAR.md].

    [of_pgm] is what {!Rust}'s tasks hand to the interpreter; [to_pgm] is its
    inverse, used by {!Unparse} so that the printer stays a total function of a
    typed tree rather than a match on stringly-typed atoms. *)

open Common.Source
open Lang.Il
open Lang.Il.Case
module V = Lang.Il.Value

(* ------------------------------------------------------------------ *)
(* Constructors                                                        *)
(* ------------------------------------------------------------------ *)

let tv (tid : string) : typ = Typ.var tid [] $ no_region
let case (var : string) (tag : string) (args : V.t list) : V.t =
  (kw tag :: List.map arg args) |> case_v ~var

let vlist (tid : string) (vs : V.t list) : V.t = V.list (tv tid) vs
let vtexts (ss : string list) : V.t = V.list (Typ.text $ no_region) (List.map V.text ss)
let vnat (n : Bigint.t) : V.t = V.nat n
let no_label : V.t = V.opt (Typ.nat $ no_region) None

(* ------------------------------------------------------------------ *)
(* Destructors                                                         *)
(* ------------------------------------------------------------------ *)

exception Bad of string

let flat (v : V.t) : string * string list * V.t list =
  try V.flatten_case_v v with _ -> raise (Bad "expected a constructor value")

let bad what atoms = raise (Bad (what ^ ": " ^ String.concat ";" atoms))
let texts (v : V.t) : string list = List.map V.get_text (V.get_list v)
let nat (v : V.t) : Bigint.t = Xl.Num.to_int (V.get_num v)

(* ------------------------------------------------------------------ *)
(* Crate labels                                                        *)
(* ------------------------------------------------------------------ *)

let atom (s : string) = Xl.Atom.keyword s $ no_region

let of_locality = function
  | Ast.Local -> case "locality" "LOCAL" []
  | Ast.Foreign -> case "locality" "FOREIGN" []

let to_locality v =
  match flat v with
  | _, [ "LOCAL" ], [] -> Ast.Local
  | _, [ "FOREIGN" ], [] -> Ast.Foreign
  | _, a, _ -> bad "locality" a

let of_edition = function
  | Ast.E2021 -> case "edition" "E2021" []
  | Ast.E2024 -> case "edition" "E2024" []

let to_edition v =
  match flat v with
  | _, [ "E2021" ], [] -> Ast.E2021
  | _, [ "E2024" ], [] -> Ast.E2024
  | _, a, _ -> bad "edition" a

let of_crateid (c : Ast.crateid) : V.t =
  V.record "crateid"
    [
      (atom "CNAME", V.text c.Ast.cname);
      (atom "CLOC", of_locality c.Ast.cloc);
      (atom "CED", of_edition c.Ast.ced);
    ]

let to_crateid (v : V.t) : Ast.crateid =
  match V.get_struct v with
  | [ (_, n); (_, l); (_, e) ] ->
      { Ast.cname = V.get_text n; cloc = to_locality l; ced = to_edition e }
  | _ -> raise (Bad "crateid")

(* ------------------------------------------------------------------ *)
(* Regions                                                             *)
(* ------------------------------------------------------------------ *)

let of_region = function
  | Ast.RName r -> case "region" "RNAME" [ V.text r ]
  | Ast.RStatic -> case "region" "RSTATIC" []
  | Ast.RAnon -> case "region" "RANON" []

let to_region v =
  match flat v with
  | _, [ "RNAME" ], [ r ] -> Ast.RName (V.get_text r)
  | _, [ "RSTATIC" ], [] -> Ast.RStatic
  | _, [ "RANON" ], [] -> Ast.RAnon
  | _, a, _ -> bad "region" a

let of_regions rs = vlist "region" (List.map of_region rs)
let to_regions v = List.map to_region (V.get_list v)

(* ------------------------------------------------------------------ *)
(* Types, bounds                                                       *)
(* ------------------------------------------------------------------ *)

let rec of_typ (t : Ast.typ) : V.t =
  let c = case "typ" in
  match t with
  | Ast.Unit -> c "UNIT" []
  | Ast.BoolT -> c "BOOLT" []
  | Ast.I32 -> c "I32" []
  | Ast.U8 -> c "U8" []
  | Ast.Usize -> c "USIZE" []
  | Ast.Str -> c "STR" []
  | Ast.TParam t -> c "TPARAM" [ V.text t ]
  | Ast.Ref (r, t) -> c "REF" [ of_region r; of_typ t ]
  | Ast.RefMut (r, t) -> c "REFMUT" [ of_region r; of_typ t ]
  | Ast.FnPtr (rs, ts, t) -> c "FNPTR" [ vtexts rs; of_typs ts; of_typ t ]
  | Ast.Adt (n, ts, rs) -> c "ADT" [ V.text n; of_typs ts; of_regions rs ]
  | Ast.Tup ts -> c "TUP" [ of_typs ts ]
  | Ast.Arr (t, n) -> c "ARR" [ of_typ t; vnat n ]
  | Ast.DynW (bs, r) ->
      c "DYNW"
        [ of_bounds bs; V.opt (tv "region") (Option.map of_region r) ]
  | Ast.Proj (t, tr, a) -> c "PROJ" [ of_typ t; of_traitref tr; V.text a ]
  | Ast.ImplT (bs, cap) -> c "IMPLT" [ no_label; of_bounds bs; of_cap cap ]
  | Ast.Hole -> c "HOLE" [ no_label ]
  | Ast.AliasU (n, ts, rs) -> c "ALIASU" [ V.text n; of_typs ts; of_regions rs ]
  | Ast.TShort (t, a) -> c "TSHORT" [ of_typ t; V.text a ]
  | Ast.TName _ -> raise (Bad "an unresolved type name reached the converter")

and of_typs ts = vlist "typ" (List.map of_typ ts)

and of_traitref (Ast.TR (d, ts, rs)) =
  case "traitref" "TR" [ V.text d; of_typs ts; of_regions rs ]

and of_bound (b : Ast.bound) : V.t =
  let c = case "bound" in
  match b with
  | Ast.BTrait (d, ts, rs, cs) ->
      c "BTRAIT" [ V.text d; of_typs ts; of_regions rs; of_constrs cs ]
  | Ast.BRegion r -> c "BREGION" [ of_region r ]
  | Ast.BRelax -> c "BRELAX" []
  | Ast.BForall (rs, b) -> c "BFORALL" [ vtexts rs; of_bound b ]

and of_bounds bs = vlist "bound" (List.map of_bound bs)

and of_constr = function
  | Ast.CEq (a, t) -> case "constr" "CEQ" [ V.text a; of_typ t ]
  | Ast.CBnd (a, bs) -> case "constr" "CBND" [ V.text a; of_bounds bs ]

and of_constrs cs = vlist "constr" (List.map of_constr cs)

and of_cap = function
  | Ast.CapNone -> case "cap" "CAPNONE" []
  | Ast.CapUse (ts, rs) -> case "cap" "CAPUSE" [ vtexts ts; vtexts rs ]

let rec to_typ (v : V.t) : Ast.typ =
  match flat v with
  | _, [ "UNIT" ], [] -> Ast.Unit
  | _, [ "BOOLT" ], [] -> Ast.BoolT
  | _, [ "I32" ], [] -> Ast.I32
  | _, [ "U8" ], [] -> Ast.U8
  | _, [ "USIZE" ], [] -> Ast.Usize
  | _, [ "STR" ], [] -> Ast.Str
  | _, [ "TPARAM" ], [ t ] -> Ast.TParam (V.get_text t)
  | _, [ "REF" ], [ r; t ] -> Ast.Ref (to_region r, to_typ t)
  | _, [ "REFMUT" ], [ r; t ] -> Ast.RefMut (to_region r, to_typ t)
  | _, [ "FNPTR" ], [ rs; ts; t ] -> Ast.FnPtr (texts rs, to_typs ts, to_typ t)
  | _, [ "ADT" ], [ n; ts; rs ] ->
      Ast.Adt (V.get_text n, to_typs ts, to_regions rs)
  | _, [ "TUP" ], [ ts ] -> Ast.Tup (to_typs ts)
  | _, [ "ARR" ], [ t; n ] -> Ast.Arr (to_typ t, nat n)
  | _, [ "DYNW" ], [ bs; r ] ->
      Ast.DynW (to_bounds bs, Option.map to_region (V.get_opt r))
  | _, [ "PROJ" ], [ t; tr; a ] ->
      Ast.Proj (to_typ t, to_traitref tr, V.get_text a)
  | _, [ "IMPLT" ], [ _; bs; cap ] -> Ast.ImplT (to_bounds bs, to_cap cap)
  | _, [ "HOLE" ], [ _ ] -> Ast.Hole
  | _, [ "ALIASU" ], [ n; ts; rs ] ->
      Ast.AliasU (V.get_text n, to_typs ts, to_regions rs)
  | _, [ "TSHORT" ], [ t; a ] -> Ast.TShort (to_typ t, V.get_text a)
  | _, a, _ -> bad "typ" a

and to_typs v = List.map to_typ (V.get_list v)

and to_traitref v =
  match flat v with
  | _, [ "TR" ], [ d; ts; rs ] ->
      Ast.TR (V.get_text d, to_typs ts, to_regions rs)
  | _, a, _ -> bad "traitref" a

and to_bound v =
  match flat v with
  | _, [ "BTRAIT" ], [ d; ts; rs; cs ] ->
      Ast.BTrait (V.get_text d, to_typs ts, to_regions rs, to_constrs cs)
  | _, [ "BREGION" ], [ r ] -> Ast.BRegion (to_region r)
  | _, [ "BRELAX" ], [] -> Ast.BRelax
  | _, [ "BFORALL" ], [ rs; b ] -> Ast.BForall (texts rs, to_bound b)
  | _, a, _ -> bad "bound" a

and to_bounds v = List.map to_bound (V.get_list v)

and to_constr v =
  match flat v with
  | _, [ "CEQ" ], [ a; t ] -> Ast.CEq (V.get_text a, to_typ t)
  | _, [ "CBND" ], [ a; bs ] -> Ast.CBnd (V.get_text a, to_bounds bs)
  | _, a, _ -> bad "constr" a

and to_constrs v = List.map to_constr (V.get_list v)

and to_cap v =
  match flat v with
  | _, [ "CAPNONE" ], [] -> Ast.CapNone
  | _, [ "CAPUSE" ], [ ts; rs ] -> Ast.CapUse (texts ts, texts rs)
  | _, a, _ -> bad "cap" a

(* ------------------------------------------------------------------ *)
(* Patterns, literals, paths                                           *)
(* ------------------------------------------------------------------ *)

let rec of_pat (q : Ast.pat) : V.t =
  let c = case "pat" in
  match q with
  | Ast.QVar x -> c "QVAR" [ V.text x ]
  | Ast.QWild -> c "QWILD" []
  | Ast.QVariant (e, v, qs) -> c "QVARIANT" [ V.text e; V.text v; of_pats qs ]
  | Ast.QTupStruct (s, qs) -> c "QTUPSTRUCT" [ V.text s; of_pats qs ]

and of_pats qs = vlist "pat" (List.map of_pat qs)

let rec to_pat v =
  match flat v with
  | _, [ "QVAR" ], [ x ] -> Ast.QVar (V.get_text x)
  | _, [ "QWILD" ], [] -> Ast.QWild
  | _, [ "QVARIANT" ], [ e; n; qs ] ->
      Ast.QVariant (V.get_text e, V.get_text n, to_pats qs)
  | _, [ "QTUPSTRUCT" ], [ s; qs ] ->
      Ast.QTupStruct (V.get_text s, to_pats qs)
  | _, a, _ -> bad "pat" a

and to_pats v = List.map to_pat (V.get_list v)

let of_lit (l : Ast.lit) : V.t =
  let c = case "lit" in
  match l with
  | Ast.LNum n -> c "LNUM" [ no_label; vnat n ]
  | Ast.LI32 n -> c "LI32" [ vnat n ]
  | Ast.LU8 n -> c "LU8" [ vnat n ]
  | Ast.LUsize n -> c "LUSIZE" [ vnat n ]
  | Ast.LTrue -> c "LTRUE" []
  | Ast.LFalse -> c "LFALSE" []
  | Ast.LUnit -> c "LUNIT" []

let to_lit v =
  match flat v with
  | _, [ "LNUM" ], [ _; n ] -> Ast.LNum (nat n)
  | _, [ "LI32" ], [ n ] -> Ast.LI32 (nat n)
  | _, [ "LU8" ], [ n ] -> Ast.LU8 (nat n)
  | _, [ "LUSIZE" ], [ n ] -> Ast.LUsize (nat n)
  | _, [ "LTRUE" ], [] -> Ast.LTrue
  | _, [ "LFALSE" ], [] -> Ast.LFalse
  | _, [ "LUNIT" ], [] -> Ast.LUnit
  | _, a, _ -> bad "lit" a

let of_path (p : Ast.path) : V.t =
  let c = case "path" in
  match p with
  | Ast.PFn f -> c "PFN" [ V.text f ]
  | Ast.PStruct s -> c "PSTRUCT" [ V.text s ]
  | Ast.PVariant (e, v) -> c "PVARIANT" [ V.text e; V.text v ]
  | Ast.PConst n -> c "PCONST" [ V.text n ]
  | Ast.PQFn (t, tr, f) -> c "PQFN" [ of_typ t; of_traitref tr; V.text f ]
  | Ast.PQConst (t, tr, n) -> c "PQCONST" [ of_typ t; of_traitref tr; V.text n ]
  | Ast.PBVariant v -> c "PBVARIANT" [ V.text v ]
  | Ast.PShort (t, x) -> c "PSHORT" [ of_typ t; V.text x ]
  | Ast.PDShort (d, f) -> c "PDSHORT" [ V.text d; V.text f ]
  | Ast.PName _ | Ast.PName2 _ | Ast.PQual _ ->
      raise (Bad "an unresolved path reached the converter")

let to_path v =
  match flat v with
  | _, [ "PFN" ], [ f ] -> Ast.PFn (V.get_text f)
  | _, [ "PSTRUCT" ], [ s ] -> Ast.PStruct (V.get_text s)
  | _, [ "PVARIANT" ], [ e; n ] -> Ast.PVariant (V.get_text e, V.get_text n)
  | _, [ "PCONST" ], [ n ] -> Ast.PConst (V.get_text n)
  | _, [ "PQFN" ], [ t; tr; f ] ->
      Ast.PQFn (to_typ t, to_traitref tr, V.get_text f)
  | _, [ "PQCONST" ], [ t; tr; n ] ->
      Ast.PQConst (to_typ t, to_traitref tr, V.get_text n)
  | _, [ "PBVARIANT" ], [ n ] -> Ast.PBVariant (V.get_text n)
  | _, [ "PSHORT" ], [ t; x ] -> Ast.PShort (to_typ t, V.get_text x)
  | _, [ "PDSHORT" ], [ d; f ] -> Ast.PDShort (V.get_text d, V.get_text f)
  | _, a, _ -> bad "path" a

let of_targs = function
  | Ast.TANone -> case "targs" "TANONE" []
  | Ast.TASome (ts, rs) -> case "targs" "TASOME" [ of_typs ts; of_regions rs ]

let to_targs v =
  match flat v with
  | _, [ "TANONE" ], [] -> Ast.TANone
  | _, [ "TASOME" ], [ ts; rs ] -> Ast.TASome (to_typs ts, to_regions rs)
  | _, a, _ -> bad "targs" a

let of_mv = function
  | Ast.MvNone -> case "mv" "MVNONE" []
  | Ast.MvMove -> case "mv" "MVMOVE" []

let to_mv v =
  match flat v with
  | _, [ "MVNONE" ], [] -> Ast.MvNone
  | _, [ "MVMOVE" ], [] -> Ast.MvMove
  | _, a, _ -> bad "mv" a

(* ------------------------------------------------------------------ *)
(* Terms                                                               *)
(* ------------------------------------------------------------------ *)

let of_rett = function
  | Ast.RtNone -> case "rett" "RTNONE" []
  | Ast.RtSome t -> case "rett" "RTSOME" [ of_typ t ]

let to_rett v =
  match flat v with
  | _, [ "RTNONE" ], [] -> Ast.RtNone
  | _, [ "RTSOME" ], [ t ] -> Ast.RtSome (to_typ t)
  | _, a, _ -> bad "rett" a

let of_cparam = function
  | Ast.CaPat q -> case "cparam" "CAPAT" [ of_pat q ]
  | Ast.CaPatT (q, t) -> case "cparam" "CAPATT" [ of_pat q; of_typ t ]

let to_cparam v =
  match flat v with
  | _, [ "CAPAT" ], [ q ] -> Ast.CaPat (to_pat q)
  | _, [ "CAPATT" ], [ q; t ] -> Ast.CaPatT (to_pat q, to_typ t)
  | _, a, _ -> bad "cparam" a

let rec of_term (e : Ast.term) : V.t =
  let c = case "term" in
  match e with
  | Ast.EVar x -> c "EVAR" [ V.text x ]
  | Ast.EPath (p, ta) -> c "EPATH" [ no_label; of_path p; of_targs ta ]
  | Ast.ELit l -> c "ELIT" [ of_lit l ]
  | Ast.ECall (e, es) -> c "ECALL" [ of_term e; of_terms es ]
  | Ast.ETup es -> c "ETUP" [ of_terms es ]
  | Ast.EStruct (s, ta, fs) ->
      c "ESTRUCT" [ no_label; V.text s; of_targs ta; of_fields fs ]
  | Ast.EArray es -> c "EARRAY" [ of_terms es ]
  | Ast.ERepeat (e, n) -> c "EREPEAT" [ of_term e; vnat n ]
  | Ast.ERef e -> c "EREF" [ of_term e ]
  | Ast.ERefMut e -> c "EREFMUT" [ of_term e ]
  | Ast.EDeref e -> c "EDEREF" [ of_term e ]
  | Ast.EField (e, x) -> c "EFIELD" [ of_term e; V.text x ]
  | Ast.EAssign (l, e) -> c "EASSIGN" [ of_lv l; of_term e ]
  | Ast.ECast (e, t) -> c "ECAST" [ of_term e; of_typ t ]
  | Ast.ETry e -> c "ETRY" [ of_term e ]
  | Ast.EClosure (m, ps, r, e) ->
      c "ECLOSURE"
        [ of_mv m; no_label; vlist "cparam" (List.map of_cparam ps);
          of_rett r; of_term e ]
  | Ast.EAsync (m, e) -> c "EASYNC" [ no_label; of_mv m; of_term e ]
  | Ast.EAwait e -> c "EAWAIT" [ of_term e ]
  | Ast.EMatch (e, arms) ->
      c "EMATCH"
        [ of_term e;
          vlist "arm"
            (List.map (fun (q, e) -> case "arm" "ARM" [ of_pat q; of_term e ]) arms) ]
  | Ast.ELoop e -> c "ELOOP" [ no_label; of_term e ]
  | Ast.EReturn e -> c "ERETURN" [ no_label; of_term e ]
  | Ast.ELet (q, t, e1, e2) ->
      c "ELET" [ of_pat q; of_typ t; of_term e1; of_term e2 ]
  | Ast.ESeq (e1, e2) -> c "ESEQ" [ of_term e1; of_term e2 ]
  | Ast.EIdent _ -> raise (Bad "an unresolved name reached the converter")

and of_terms es = vlist "term" (List.map of_term es)

and of_fields fs =
  vlist "field"
    (List.map (fun (x, e) -> case "field" "FLD" [ V.text x; of_term e ]) fs)

and of_lv = function
  | Ast.LvVar x -> case "lv" "LVVAR" [ V.text x ]
  | Ast.LvDeref e -> case "lv" "LVDEREF" [ of_term e ]
  | Ast.LvField (e, x) -> case "lv" "LVFIELD" [ of_term e; V.text x ]

let rec to_term v =
  match flat v with
  | _, [ "EVAR" ], [ x ] -> Ast.EVar (V.get_text x)
  | _, [ "EPATH" ], [ _; p; ta ] -> Ast.EPath (to_path p, to_targs ta)
  | _, [ "ELIT" ], [ l ] -> Ast.ELit (to_lit l)
  | _, [ "ECALL" ], [ e; es ] -> Ast.ECall (to_term e, to_terms es)
  | _, [ "ETUP" ], [ es ] -> Ast.ETup (to_terms es)
  | _, [ "ESTRUCT" ], [ _; s; ta; fs ] ->
      Ast.EStruct (V.get_text s, to_targs ta, to_fields fs)
  | _, [ "EARRAY" ], [ es ] -> Ast.EArray (to_terms es)
  | _, [ "EREPEAT" ], [ e; n ] -> Ast.ERepeat (to_term e, nat n)
  | _, [ "EREF" ], [ e ] -> Ast.ERef (to_term e)
  | _, [ "EREFMUT" ], [ e ] -> Ast.ERefMut (to_term e)
  | _, [ "EDEREF" ], [ e ] -> Ast.EDeref (to_term e)
  | _, [ "EFIELD" ], [ e; x ] -> Ast.EField (to_term e, V.get_text x)
  | _, [ "EASSIGN" ], [ l; e ] -> Ast.EAssign (to_lv l, to_term e)
  | _, [ "ECAST" ], [ e; t ] -> Ast.ECast (to_term e, to_typ t)
  | _, [ "ETRY" ], [ e ] -> Ast.ETry (to_term e)
  | _, [ "ECLOSURE" ], [ m; _; ps; r; e ] ->
      Ast.EClosure
        (to_mv m, List.map to_cparam (V.get_list ps), to_rett r, to_term e)
  | _, [ "EASYNC" ], [ _; m; e ] -> Ast.EAsync (to_mv m, to_term e)
  | _, [ "EAWAIT" ], [ e ] -> Ast.EAwait (to_term e)
  | _, [ "EMATCH" ], [ e; arms ] ->
      Ast.EMatch
        ( to_term e,
          List.map
            (fun a ->
              match flat a with
              | _, [ "ARM" ], [ q; e ] -> (to_pat q, to_term e)
              | _, at, _ -> bad "arm" at)
            (V.get_list arms) )
  | _, [ "ELOOP" ], [ _; e ] -> Ast.ELoop (to_term e)
  | _, [ "ERETURN" ], [ _; e ] -> Ast.EReturn (to_term e)
  | _, [ "ELET" ], [ q; t; e1; e2 ] ->
      Ast.ELet (to_pat q, to_typ t, to_term e1, to_term e2)
  | _, [ "ESEQ" ], [ e1; e2 ] -> Ast.ESeq (to_term e1, to_term e2)
  | _, a, _ -> bad "term" a

and to_terms v = List.map to_term (V.get_list v)

and to_fields v =
  List.map
    (fun f ->
      match flat f with
      | _, [ "FLD" ], [ x; e ] -> (V.get_text x, to_term e)
      | _, a, _ -> bad "field" a)
    (V.get_list v)

and to_lv v =
  match flat v with
  | _, [ "LVVAR" ], [ x ] -> Ast.LvVar (V.get_text x)
  | _, [ "LVDEREF" ], [ e ] -> Ast.LvDeref (to_term e)
  | _, [ "LVFIELD" ], [ e; x ] -> Ast.LvField (to_term e, V.get_text x)
  | _, a, _ -> bad "lv" a

(* ------------------------------------------------------------------ *)
(* Items                                                               *)
(* ------------------------------------------------------------------ *)

let of_vis = function
  | Ast.VPriv -> case "vis" "VPRIV" []
  | Ast.VPub -> case "vis" "VPUB" []

let to_vis v =
  match flat v with
  | _, [ "VPRIV" ], [] -> Ast.VPriv
  | _, [ "VPUB" ], [] -> Ast.VPub
  | _, a, _ -> bad "vis" a

let of_aq = function
  | Ast.AqNone -> case "aq" "AQNONE" []
  | Ast.AqAsync -> case "aq" "AQASYNC" [ no_label ]

let to_aq v =
  match flat v with
  | _, [ "AQNONE" ], [] -> Ast.AqNone
  | _, [ "AQASYNC" ], [ _ ] -> Ast.AqAsync
  | _, a, _ -> bad "aq" a

let of_gparam = function
  | Ast.GTy t -> case "gparam" "GTY" [ V.text t ]
  | Ast.GTyB (t, bs) -> case "gparam" "GTYB" [ V.text t; of_bounds bs ]
  | Ast.GRg r -> case "gparam" "GRG" [ V.text r ]
  | Ast.GRgB (r, rs) -> case "gparam" "GRGB" [ V.text r; vtexts rs ]

let of_gparams gs = vlist "gparam" (List.map of_gparam gs)

let to_gparam v =
  match flat v with
  | _, [ "GTY" ], [ t ] -> Ast.GTy (V.get_text t)
  | _, [ "GTYB" ], [ t; bs ] -> Ast.GTyB (V.get_text t, to_bounds bs)
  | _, [ "GRG" ], [ r ] -> Ast.GRg (V.get_text r)
  | _, [ "GRGB" ], [ r; rs ] -> Ast.GRgB (V.get_text r, texts rs)
  | _, a, _ -> bad "gparam" a

let to_gparams v = List.map to_gparam (V.get_list v)

let of_fparam = function
  | Ast.APat (q, t) -> case "fparam" "APAT" [ of_pat q; of_typ t ]
  | Ast.ASelf t -> case "fparam" "ASELF" [ of_typ t ]

let of_fparams ps = vlist "fparam" (List.map of_fparam ps)

let to_fparam v =
  match flat v with
  | _, [ "APAT" ], [ q; t ] -> Ast.APat (to_pat q, to_typ t)
  | _, [ "ASELF" ], [ t ] -> Ast.ASelf (to_typ t)
  | _, a, _ -> bad "fparam" a

let to_fparams v = List.map to_fparam (V.get_list v)

let rec of_bassert = function
  | Ast.BATy (t, bs) -> case "bassert" "BATY" [ of_typ t; of_bounds bs ]
  | Ast.BARg (r, rs) -> case "bassert" "BARG" [ of_region r; of_regions rs ]
  | Ast.BAForall (rs, b) ->
      case "bassert" "BAFORALL" [ vtexts rs; of_bassert b ]

let of_wheres w = vlist "bassert" (List.map of_bassert w)

let rec to_bassert v =
  match flat v with
  | _, [ "BATY" ], [ t; bs ] -> Ast.BATy (to_typ t, to_bounds bs)
  | _, [ "BARG" ], [ r; rs ] -> Ast.BARg (to_region r, to_regions rs)
  | _, [ "BAFORALL" ], [ rs; b ] -> Ast.BAForall (texts rs, to_bassert b)
  | _, a, _ -> bad "bassert" a

let to_wheres v = List.map to_bassert (V.get_list v)

let of_titem = function
  | Ast.TIType (a, bs, w) ->
      case "titem" "TITYPE" [ V.text a; of_bounds bs; of_wheres w ]
  | Ast.TIConst (n, t) -> case "titem" "TICONST" [ V.text n; of_typ t ]
  | Ast.TIFn (f, gs, ps, ret, w) ->
      case "titem" "TIFN"
        [ V.text f; of_gparams gs; of_fparams ps; of_typ ret; of_wheres w ]

let to_titem v =
  match flat v with
  | _, [ "TITYPE" ], [ a; bs; w ] ->
      Ast.TIType (V.get_text a, to_bounds bs, to_wheres w)
  | _, [ "TICONST" ], [ n; t ] -> Ast.TIConst (V.get_text n, to_typ t)
  | _, [ "TIFN" ], [ f; gs; ps; ret; w ] ->
      Ast.TIFn (V.get_text f, to_gparams gs, to_fparams ps, to_typ ret,
                to_wheres w)
  | _, a, _ -> bad "titem" a

let of_iitem = function
  | Ast.IIType (a, t, w) ->
      case "iitem" "IITYPE" [ V.text a; of_typ t; of_wheres w ]
  | Ast.IIConst (n, t, e) ->
      case "iitem" "IICONST" [ V.text n; of_typ t; of_term e ]
  | Ast.IIFn (f, gs, ps, ret, w, e) ->
      case "iitem" "IIFN"
        [ V.text f; of_gparams gs; of_fparams ps; of_typ ret; of_wheres w;
          of_term e ]

let to_iitem v =
  match flat v with
  | _, [ "IITYPE" ], [ a; t; w ] ->
      Ast.IIType (V.get_text a, to_typ t, to_wheres w)
  | _, [ "IICONST" ], [ n; t; e ] ->
      Ast.IIConst (V.get_text n, to_typ t, to_term e)
  | _, [ "IIFN" ], [ f; gs; ps; ret; w; e ] ->
      Ast.IIFn (V.get_text f, to_gparams gs, to_fparams ps, to_typ ret,
                to_wheres w, to_term e)
  | _, a, _ -> bad "iitem" a

let of_item (it : Ast.item) : V.t =
  let c = case "item" in
  match it with
  | Ast.IStruct (v, s, gs, w, fs) ->
      c "ISTRUCT"
        [ of_vis v; V.text s; of_gparams gs; of_wheres w;
          vlist "sfield"
            (List.map (fun (Ast.SF (x, t)) ->
                 case "sfield" "SF" [ V.text x; of_typ t ]) fs) ]
  | Ast.ITupStruct (v, s, gs, w, ts) ->
      c "ITUPSTRUCT" [ of_vis v; V.text s; of_gparams gs; of_wheres w; of_typs ts ]
  | Ast.IEnum (v, e, gs, w, vs) ->
      c "IENUM"
        [ of_vis v; V.text e; of_gparams gs; of_wheres w;
          vlist "variant"
            (List.map (fun (Ast.VAR (n, ts)) ->
                 case "variant" "VAR" [ V.text n; of_typs ts ]) vs) ]
  | Ast.ITrait (v, d, gs, sup, w, its) ->
      c "ITRAIT"
        [ of_vis v; V.text d; of_gparams gs; of_bounds sup; of_wheres w;
          vlist "titem" (List.map of_titem its) ]
  | Ast.IImpl (v, gs, tr, self, w, its) ->
      c "IIMPL"
        [ of_vis v; of_gparams gs; of_traitref tr; of_typ self; of_wheres w;
          vlist "iitem" (List.map of_iitem its) ]
  | Ast.IFn (v, aq, f, gs, ps, ret, w, e) ->
      c "IFN"
        [ of_vis v; of_aq aq; V.text f; of_gparams gs; of_fparams ps;
          of_typ ret; of_wheres w; of_term e ]
  | Ast.IConst (v, n, t, e) ->
      c "ICONST" [ of_vis v; V.text n; of_typ t; of_term e ]
  | Ast.IAlias (v, a, gs, t) ->
      c "IALIAS" [ of_vis v; V.text a; of_gparams gs; of_typ t ]

let to_item v =
  match flat v with
  | _, [ "ISTRUCT" ], [ vis; s; gs; w; fs ] ->
      Ast.IStruct
        ( to_vis vis, V.get_text s, to_gparams gs, to_wheres w,
          List.map
            (fun f ->
              match flat f with
              | _, [ "SF" ], [ x; t ] -> Ast.SF (V.get_text x, to_typ t)
              | _, a, _ -> bad "sfield" a)
            (V.get_list fs) )
  | _, [ "ITUPSTRUCT" ], [ vis; s; gs; w; ts ] ->
      Ast.ITupStruct (to_vis vis, V.get_text s, to_gparams gs, to_wheres w,
                      to_typs ts)
  | _, [ "IENUM" ], [ vis; e; gs; w; vs ] ->
      Ast.IEnum
        ( to_vis vis, V.get_text e, to_gparams gs, to_wheres w,
          List.map
            (fun x ->
              match flat x with
              | _, [ "VAR" ], [ n; ts ] -> Ast.VAR (V.get_text n, to_typs ts)
              | _, a, _ -> bad "variant" a)
            (V.get_list vs) )
  | _, [ "ITRAIT" ], [ vis; d; gs; sup; w; its ] ->
      Ast.ITrait (to_vis vis, V.get_text d, to_gparams gs, to_bounds sup,
                  to_wheres w, List.map to_titem (V.get_list its))
  | _, [ "IIMPL" ], [ vis; gs; tr; self; w; its ] ->
      Ast.IImpl (to_vis vis, to_gparams gs, to_traitref tr, to_typ self,
                 to_wheres w, List.map to_iitem (V.get_list its))
  | _, [ "IFN" ], [ vis; aq; f; gs; ps; ret; w; e ] ->
      Ast.IFn (to_vis vis, to_aq aq, V.get_text f, to_gparams gs,
               to_fparams ps, to_typ ret, to_wheres w, to_term e)
  | _, [ "ICONST" ], [ vis; n; t; e ] ->
      Ast.IConst (to_vis vis, V.get_text n, to_typ t, to_term e)
  | _, [ "IALIAS" ], [ vis; a; gs; t ] ->
      Ast.IAlias (to_vis vis, V.get_text a, to_gparams gs, to_typ t)
  | _, a, _ -> bad "item" a

(* ------------------------------------------------------------------ *)
(* Programs                                                            *)
(* ------------------------------------------------------------------ *)

let of_crate (Ast.CRATE (c, its)) =
  case "crate" "CRATE" [ of_crateid c; vlist "item" (List.map of_item its) ]

let to_crate v =
  match flat v with
  | _, [ "CRATE" ], [ c; its ] ->
      Ast.CRATE (to_crateid c, List.map to_item (V.get_list its))
  | _, a, _ -> bad "crate" a

let of_pgm (Ast.PGM cs) =
  case "pgm" "PGM" [ vlist "crate" (List.map of_crate cs) ]

let to_pgm v =
  match flat v with
  | _, [ "PGM" ], [ cs ] -> Ast.PGM (List.map to_crate (V.get_list cs))
  | _, a, _ -> bad "pgm" a
