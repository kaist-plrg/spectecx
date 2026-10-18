open Common.Source
open Envs_make
open Lang.Il

let ( let* ) = Result.bind

let rec check ~opaque_types ~find_typdef_opt ~typ value =
  let check = check ~opaque_types ~find_typdef_opt in
  let check_values = check_values ~opaque_types ~find_typdef_opt in
  let mismatch () =
    Error
      (Printf.sprintf "expected %s, got %s" (Print.string_of_typ typ)
         (Print.string_of_value ~short:true value))
  in
  match (typ.it, value.it) with
  | BoolT, BoolV _ | TextT, TextV _ | FuncT, FuncV _ -> Ok ()
  | NumT `IntT, NumV _ -> Ok ()
  | NumT `NatT, NumV (`Nat n | `Int n) when Bigint.(n >= zero) -> Ok ()
  | VarT { synid; targs }, _ -> (
      match find_typdef_opt synid with
      | None -> Error (Printf.sprintf "type %s is undefined" synid.it)
      | Some (tparams, deftyp) -> (
          if List.length tparams <> List.length targs then mismatch ()
          else
            let theta = List.combine tparams targs |> TIdMap.of_list in
            let subst = Envs_il.Typ.subst_typ theta in
            match (deftyp.it, value.it) with
            | OpaqueT, OpaqueV payload ->
                Opaque_types.check_type opaque_types ~name:synid.it
                  ~value:payload
            | PlainT inner, _ -> check ~typ:(subst inner) value
            | StructT fields, StructV values
              when List.length fields = List.length values ->
                List.fold_left2
                  (fun result (atom, typ) (actual, value) ->
                    let* () = result in
                    if Lang.Xl.Atom.eq atom.it actual.it then
                      check ~typ:(subst typ) value
                    else mismatch ())
                  (Ok ()) fields values
            | VariantT cases, CaseV actual -> (
                match
                  List.find_opt
                    (fun case -> Mixfix.eq_mixop case.notation.it actual)
                    cases
                with
                | Some case ->
                    check_values
                      ~typs:(List.map subst (Mixfix.args case.notation.it))
                      (Mixfix.args actual)
                | None -> mismatch ())
            | _ -> mismatch ()))
  | TupleT typs, TupleV values -> check_values ~typs values
  | IterT { iter = Opt; _ }, OptV None -> Ok ()
  | IterT { typ; iter = Opt }, OptV (Some value) -> check ~typ value
  | IterT { typ; iter = List }, ListV values ->
      check_values ~typs:(List.map (fun _ -> typ) values) values
  | _ -> mismatch ()

and check_values ~opaque_types ~find_typdef_opt ~typs values =
  let expected = List.length typs in
  let actual = List.length values in
  if expected <> actual then
    Error (Printf.sprintf "arity mismatch: expected %d, got %d" expected actual)
  else
    List.fold_left2
      (fun result typ value ->
        let* () = result in
        check ~opaque_types ~find_typdef_opt ~typ value)
      (Ok ()) typs values
