type error = ParseError of Parse.error | SemanticError of Diag.t list
type spec_source = Parse.spec_source = { filename : string; contents : string }

let parse_files filenames =
  Parse.parse_files filenames |> Result.map_error (fun e -> ParseError e)

let parse_source source =
  Parse.parse_source source |> Result.map_error (fun e -> ParseError e)

let parse_sources sources =
  Parse.parse_sources sources |> Result.map_error (fun e -> ParseError e)

let elaborate_and_algorithmize spec_el =
  let spec_il, errors_elab = Elaborate.elab_spec spec_el in
  let spec_al, errors_algo = Algo.algo_spec spec_il in
  match errors_algo @ errors_elab with
  | [] -> Ok spec_al
  | errors -> Error (SemanticError errors)

let structure spec = Structure.struct_spec spec
let henv_of_el_spec spec = Hints.Henv.of_el_spec spec
let henv_with_al_spec henv spec_al = Hints.Henv.load_al_spec henv spec_al

let annotate ~henv spec_sl =
  spec_sl |> Annotate.Expand.expand_spec |> Annotate.Linearize.linearize_spec
  |> Annotate.annotate_spec henv

let shorten spec = Annotate.Shorthand.shorten_spec spec

let error_to_diagnostics = function
  | ParseError e -> Diag.Bag.singleton (Parse.error_to_diagnostic e)
  | SemanticError errors -> Diag.Bag.of_list errors
