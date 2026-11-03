open Common.Annotated
module Location = Common.Location
module Il = Lang.Il
open Il.Expression

let location line =
  let left = { Location.line; column = 0 } in
  Location.Span
    { file = "annotations.spectec"; left; right = { left with column = 1 } }

let line_of location =
  match location with
  | Location.Span { left; _ } -> left.line
  | Location.Unknown | Location.File _ -> assert false

let annotated ~line ~note it = { it; at = location line; note }
let exp ~line ~typ it : Il.exp = annotated ~line ~note:{ typ; annot = () } it
let nat ~line n = exp ~line ~typ:Il.Typ.nat (NumE (`Nat (Bigint.of_int n)))

let () =
  let record_typ = Il.Typ.var "record" [] in
  let records_typ = Il.Typ.list (record_typ $ location 11) in
  let field = Xl.Atom.keyword "FIELD" $ location 12 in
  let root = annotated ~line:13 ~note:records_typ RootP in
  let slice =
    annotated ~line:14 ~note:records_typ
      (SliceP (root, nat ~line:1 0, nat ~line:2 2))
  in
  let index =
    annotated ~line:15 ~note:record_typ (IdxP (slice, nat ~line:3 1))
  in
  let path = annotated ~line:16 ~note:Il.Typ.nat (DotP (index, field)) in
  let base = exp ~line:4 ~typ:records_typ (VarE ("records" $ location 17)) in
  let arg = annotated ~line:18 ~note:() (ExpA (nat ~line:5 9)) in
  let def_arg = annotated ~line:20 ~note:() (DefA ("identity" $ location 19)) in
  let replacement =
    exp ~line:6 ~typ:Il.Typ.nat
      (CallE
         ("apply" $ location 21, [ Il.Typ.nat $ location 22 ], [ arg; def_arg ]))
  in
  let update = exp ~line:7 ~typ:records_typ (UpdE (base, path, replacement)) in
  let case_typ = Il.Typ.var "number" [] in
  let case =
    exp ~line:9 ~typ:case_typ
      (CaseE [ Il.Mixfix.Atom field; Il.Mixfix.Arg (nat ~line:8 42) ])
  in
  let original =
    exp ~line:10
      ~typ:(Il.Typ.tuple [ records_typ $ location 23; case_typ $ location 24 ])
      (TupleE [ update; case ])
  in
  let seen = ref [] in
  let annotated : int Il.Expression.t =
    Il.Expression.map_annotations
      (fun exp ->
        let line = line_of exp.at in
        seen := line :: !seen;
        line)
      original
  in
  assert (List.sort compare !seen = List.init 10 (fun i -> i + 1));
  let restored : Il.exp =
    Il.Expression.map_annotations
      (fun exp -> assert (exp.note.annot = line_of exp.at))
      annotated
  in
  assert (original = restored)
