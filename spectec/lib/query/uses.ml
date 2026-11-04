module El = Lang.El
module Location = Common.Location

type use = { owner : Definitions.definition; location : Location.t }
type occurrence = { name : string; location : Location.t; variable : bool }
type t = (occurrence * Definitions.definition) list

let add_occurrence ?(variable = false) acc ~name ~location =
  { name; location; variable } :: acc

let atom_uses acc (atom : El.atom) =
  match atom.it with
  | Lang.Xl.Atom.Keyword _ | Lang.Xl.Atom.Tag _ ->
      add_occurrence acc ~name:(El.Print.string_of_atom atom) ~location:atom.at
  | _ -> acc

let rec plaintyp_uses acc (plaintyp : El.plaintyp) =
  match plaintyp.it with
  | El.BoolT | El.NumT _ | El.TextT -> acc
  | El.VarT (id, targs) ->
      List.fold_left plaintyp_uses
        (add_occurrence acc ~name:(El.Print.string_of_typid id) ~location:id.at)
        targs
  | El.ParenT plaintyp -> plaintyp_uses acc plaintyp
  | El.TupleT plaintyps -> List.fold_left plaintyp_uses acc plaintyps
  | El.IterT (plaintyp, _) -> plaintyp_uses acc plaintyp

and typ_uses acc (typ : El.typ) =
  match typ with
  | El.PlainT plaintyp -> plaintyp_uses acc plaintyp
  | El.NotationT nottyp -> nottyp_uses acc nottyp

and nottyp_uses acc (nottyp : El.nottyp) =
  match nottyp.it with
  | El.AtomT _ -> acc
  | El.SeqT typs -> List.fold_left typ_uses acc typs
  | El.InfixT (typ_l, _atom, typ_r) -> typ_uses (typ_uses acc typ_l) typ_r
  | El.BrackT (_atom_l, typ, _atom_r) -> typ_uses acc typ

and deftyp_uses acc (deftyp : El.deftyp) =
  match deftyp.it with
  | El.OpaqueTD -> acc
  | El.PlainTD plaintyp -> plaintyp_uses acc plaintyp
  | El.StructTD typfields ->
      List.fold_left
        (fun acc (_atom, plaintyp, _hints) -> plaintyp_uses acc plaintyp)
        acc typfields
  | El.VariantTD typcases ->
      List.fold_left (fun acc (typ, _hints) -> typ_uses acc typ) acc typcases

and exp_uses acc (exp : El.exp) =
  match exp.it with
  | El.BoolE _ | El.NumE _ | El.TextE _ | El.EpsE | El.HoleE _ | El.LatexE _ ->
      acc
  | El.VarE id ->
      add_occurrence ~variable:true acc
        ~name:(El.Print.string_of_varid id)
        ~location:id.at
  | El.CallE (id, targs, args) ->
      let acc =
        add_occurrence acc ~name:(El.Print.string_of_defid id) ~location:id.at
      in
      let acc = List.fold_left plaintyp_uses acc targs in
      List.fold_left arg_uses acc args
  | El.AtomE atom -> atom_uses acc atom
  | El.UnE (_, exp)
  | El.ArithE exp
  | El.LenE exp
  | El.ParenE exp
  | El.IterE (exp, _)
  | El.UnparenE exp ->
      exp_uses acc exp
  | El.BinE (exp_l, _, exp_r)
  | El.CmpE (exp_l, _, exp_r)
  | El.ConsE (exp_l, exp_r)
  | El.CatE (exp_l, exp_r)
  | El.IdxE (exp_l, exp_r)
  | El.MemE (exp_l, exp_r)
  | El.FuseE (exp_l, exp_r) ->
      exp_uses (exp_uses acc exp_l) exp_r
  | El.SliceE (exp_1, exp_2, exp_3) ->
      exp_uses (exp_uses (exp_uses acc exp_1) exp_2) exp_3
  | El.ListE exps | El.TupleE exps | El.SeqE exps ->
      List.fold_left exp_uses acc exps
  | El.StrE fields ->
      List.fold_left
        (fun acc (atom, exp) -> exp_uses (atom_uses acc atom) exp)
        acc fields
  | El.DotE (exp, atom) -> atom_uses (exp_uses acc exp) atom
  | El.UpdE (exp_l, path, exp_r) ->
      exp_uses (path_uses (exp_uses acc exp_l) path) exp_r
  | El.SubE (exp, plaintyp) -> plaintyp_uses (exp_uses acc exp) plaintyp
  | El.InfixE (exp_l, atom, exp_r) ->
      exp_uses (atom_uses (exp_uses acc exp_l) atom) exp_r
  | El.BrackE (atom_l, exp, atom_r) ->
      atom_uses (exp_uses (atom_uses acc atom_l) exp) atom_r

and path_uses acc (path : El.path) =
  match path.it with
  | El.RootP -> acc
  | El.IdxP (path, exp) -> exp_uses (path_uses acc path) exp
  | El.SliceP (path, exp_1, exp_2) ->
      exp_uses (exp_uses (path_uses acc path) exp_1) exp_2
  | El.DotP (path, atom) -> atom_uses (path_uses acc path) atom

and arg_uses acc (arg : El.arg) =
  match arg.it with
  | El.ExpA exp -> exp_uses acc exp
  | El.DefA id ->
      add_occurrence acc ~name:(El.Print.string_of_defid id) ~location:id.at

and param_uses acc (param : El.param) =
  match param.it with
  | El.ExpP plaintyp -> plaintyp_uses acc plaintyp
  | El.DefP (_id, _tparams, params, plaintyp) ->
      plaintyp_uses (List.fold_left param_uses acc params) plaintyp

and prem_uses acc (prem : El.prem) =
  match prem.it with
  | El.VarPr (id, plaintyp) ->
      plaintyp_uses
        (add_occurrence ~variable:true acc
           ~name:(El.Print.string_of_varid id)
           ~location:id.at)
        plaintyp
  | El.RulePr (id, exp) | El.RuleNotPr (id, exp) ->
      exp_uses
        (add_occurrence acc ~name:(El.Print.string_of_relid id) ~location:id.at)
        exp
  | El.IfPr exp | El.DebugPr exp -> exp_uses acc exp
  | El.ElsePr -> acc
  | El.IterPr (prem, _) -> prem_uses acc prem

let def_uses acc (def : El.def) =
  match def.it with
  | El.SynD _ | El.SepD -> acc
  | El.TypD (_id, _tparams, deftyp, _hints) -> deftyp_uses acc deftyp
  | El.VarD (_id, plaintyp, _hints) -> plaintyp_uses acc plaintyp
  | El.RelD (_id, nottyp, _hints) -> nottyp_uses acc nottyp
  | El.RuleD (_relid, _ruleid, exp, prems) ->
      List.fold_left prem_uses (exp_uses acc exp) prems
  | El.DecD (_id, _tparams, params, plaintyp, _hints)
  | El.BuiltinDecD (_id, _tparams, params, plaintyp, _hints) ->
      plaintyp_uses (List.fold_left param_uses acc params) plaintyp
  | El.DefD (_id, _tparams, args, exp, prems) ->
      let acc = List.fold_left arg_uses acc args in
      List.fold_left prem_uses (exp_uses acc exp) prems

let of_spec (spec : El.spec) : t =
  List.concat_map
    (fun def ->
      let occurrences = List.rev (def_uses [] def) in
      List.concat_map
        (fun owner ->
          List.map (fun occurrence -> (occurrence, owner)) occurrences)
        (Definitions.of_def def))
    spec

let base_name name =
  (Lang.Xl.Var.strip_var_suffix Common.Annotated.(name $ Location.Unknown)).it

let find_all (uses : t) ~name =
  List.filter_map
    (fun ((occurrence : occurrence), owner) ->
      if
        String.equal occurrence.name name
        || (occurrence.variable && String.equal (base_name occurrence.name) name)
      then Some { owner; location = occurrence.location }
      else None)
    uses
