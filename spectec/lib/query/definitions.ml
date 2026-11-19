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

let of_spec spec = List.concat_map of_def spec

let find_all definitions ~name =
  List.filter (fun definition -> String.equal definition.name name) definitions

let find_each definitions ~names =
  let found = List.map (fun name -> (name, find_all definitions ~name)) names in
  ( List.concat_map snd found,
    List.filter_map
      (fun (name, matches) -> if matches = [] then Some name else None)
      found )

let in_files definitions ~files =
  List.filter
    (fun definition ->
      match Common.Location.file definition.location with
      | Some file -> List.mem file files
      | None -> false)
    definitions

let string_of_kind = function
  | Syntax -> "syntax"
  | Var -> "var"
  | Relation -> "relation"
  | Rule -> "rule"
  | Dec -> "dec"
  | BuiltinDec -> "builtin dec"
  | Def -> "def"
