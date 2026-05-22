type error = ParseError of Parse.error | ElaborateError of Elaborate.error

let error_to_string = function
  | ParseError e -> Parse.error_to_string e
  | ElaborateError e -> Elaborate.error_to_string e

let parse_files filenames =
  Parse.parse_files filenames |> Result.map_error (fun e -> ParseError e)

type il = Elaborate.il = { lang : Lang.Il.spec; qc : Qc_il.spec }

let elaborate spec_el =
  Elaborate.elab_spec spec_el |> Result.map_error (fun e -> ElaborateError e)

let structure spec = Structure.struct_spec spec

let error_to_diagnostics = function
  | ParseError e -> Diag.Bag.singleton (Parse.error_to_diagnostic e)
  | ElaborateError e -> Elaborate.error_to_diagnostics e
