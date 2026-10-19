open Common.Source
module Il = Lang.Il
open Il.Expression

let region line =
  let left = { file = "annotations.spectec"; line; column = 0 } in
  { left; right = { left with column = 1 } }

let phrase ~line ~note it =
  {
    it;
    at = region line;
    note;
    prov = (if line mod 2 = 0 then Some () else None);
  }

let exp ~line ~typ it : Il.exp = phrase ~line ~note:{ typ; annot = () } it
let nat ~line n = exp ~line ~typ:Il.Typ.nat (NumE (`Nat (Bigint.of_int n)))

let () =
  let record_typ = Il.Typ.var "record" [] in
  let records_typ = Il.Typ.list (record_typ $ region 11) in
  let field = Xl.Atom.keyword "FIELD" $ region 12 in
  let root = phrase ~line:13 ~note:records_typ RootP in
  let slice =
    phrase ~line:14 ~note:records_typ
      (SliceP (root, nat ~line:1 0, nat ~line:2 2))
  in
  let index = phrase ~line:15 ~note:record_typ (IdxP (slice, nat ~line:3 1)) in
  let path = phrase ~line:16 ~note:Il.Typ.nat (DotP (index, field)) in
  let base = exp ~line:4 ~typ:records_typ (VarE ("records" $ region 17)) in
  let arg = phrase ~line:18 ~note:() (ExpA (nat ~line:5 9)) in
  let def_arg = phrase ~line:20 ~note:() (DefA ("identity" $ region 19)) in
  let replacement =
    exp ~line:6 ~typ:Il.Typ.nat
      (CallE ("apply" $ region 21, [ Il.Typ.nat $ region 22 ], [ arg; def_arg ]))
  in
  let update = exp ~line:7 ~typ:records_typ (UpdE (base, path, replacement)) in
  let case_typ = Il.Typ.var "number" [] in
  let case =
    exp ~line:9 ~typ:case_typ
      (CaseE [ Il.Mixfix.Atom field; Il.Mixfix.Arg (nat ~line:8 42) ])
  in
  let original =
    exp ~line:10
      ~typ:(Il.Typ.tuple [ records_typ $ region 23; case_typ $ region 24 ])
      (TupleE [ update; case ])
  in
  let seen = ref [] in
  let annotated : int Il.Expression.t =
    Il.Expression.map_annotations
      (fun exp ->
        let line = exp.at.left.line in
        seen := line :: !seen;
        line)
      original
  in
  assert (List.sort compare !seen = List.init 10 (fun i -> i + 1));
  let restored : Il.exp =
    Il.Expression.map_annotations
      (fun exp -> assert (exp.note.annot = exp.at.left.line))
      annotated
  in
  assert (original = restored)
