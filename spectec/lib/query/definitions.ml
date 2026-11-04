module El = Lang.El

type kind = Syntax | Var | Relation | Rule | Dec | BuiltinDec | Def
type definition = { kind : kind; name : string; location : Common.Location.t }
type t = definition list

let of_def (def : El.def) =
  let definition kind name = { kind; name; location = def.at } in
  match def.it with
  | El.SynD ids ->
      List.map
        (fun ((id : El.id), _tparams) ->
          definition Syntax (El.Print.string_of_typid id))
        ids
  | El.TypD (id, _, _, _) -> [ definition Syntax (El.Print.string_of_typid id) ]
  | El.VarD (id, _, _) -> [ definition Var (El.Print.string_of_varid id) ]
  | El.RelD (id, _, _) -> [ definition Relation (El.Print.string_of_relid id) ]
  | El.RuleD (relid, ruleid, _, _) ->
      [
        definition Rule
          (El.Print.string_of_relid relid ^ El.Print.string_of_ruleid ruleid);
      ]
  | El.DecD (id, _, _, _, _) -> [ definition Dec (El.Print.string_of_defid id) ]
  | El.BuiltinDecD (id, _, _, _, _) ->
      [ definition BuiltinDec (El.Print.string_of_defid id) ]
  | El.DefD (id, _, _, _, _) -> [ definition Def (El.Print.string_of_defid id) ]
  | El.SepD -> []

let string_of_kind = function
  | Syntax -> "syntax"
  | Var -> "var"
  | Relation -> "relation"
  | Rule -> "rule"
  | Dec -> "dec"
  | BuiltinDec -> "builtin dec"
  | Def -> "def"
