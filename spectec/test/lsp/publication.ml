module Sources = Spectec_lsp.Sources
module Workspace = Spectec_lsp.Workspace
module Publication = Spectec_lsp.Publication

let update = Publication.update ~encoding:`UTF16

let () =
  let outer = Sources.canonical "overlapping/1-outer.spectec" in
  let inner = Sources.canonical "overlapping/inner/2-inner.spectec" in
  let workspace = Workspace.create () in
  let workspace = Workspace.set workspace ~path:outer "syntax outer = nat\n" in
  let workspace =
    Workspace.set workspace ~path:inner "syntax inner = missing\n"
  in
  let workspace, analyses = Workspace.affected workspace inner in
  let published, reports = update Publication.empty analyses in
  let errors = List.assoc inner reports in
  assert (List.length errors = 1);
  assert (List.assoc outer reports = []);
  let _, again = update published analyses in
  assert (again = reports);
  let _, from_empty = update Publication.empty [] in
  assert (from_empty = []);
  let outer_scope = Sources.scope_of outer in
  let inner_scope = Sources.scope_of inner in
  let without_outer, remaining = update ~closed:outer_scope published [] in
  assert (List.assoc inner remaining = errors);
  assert (List.assoc outer remaining = []);
  let _, cleared = update ~closed:inner_scope without_outer [] in
  assert (List.assoc inner cleared = []);
  let without_inner, remaining = update ~closed:inner_scope published [] in
  assert (List.assoc inner remaining = errors);
  let _, cleared = update ~closed:outer_scope without_inner [] in
  assert (List.for_all (fun (_, diagnostics) -> diagnostics = []) cleared);
  let workspace = Workspace.set workspace ~path:inner "syntax inner = nat\n" in
  let _, analyses = Workspace.affected workspace inner in
  let outer_analysis =
    List.find
      (fun (a : Workspace.analysis) -> a.sources.scope = outer_scope)
      analyses
  in
  let partial, remaining = update published [ outer_analysis ] in
  assert (List.assoc inner remaining = errors);
  let _, recovered = update partial analyses in
  assert (List.for_all (fun (_, diagnostics) -> diagnostics = []) recovered);
  let _, recovered = update ~closed:inner_scope published [ outer_analysis ] in
  assert (List.for_all (fun (_, diagnostics) -> diagnostics = []) recovered);
  let _, recovered = update published analyses in
  assert (List.for_all (fun (_, diagnostics) -> diagnostics = []) recovered);
  let _, unchanged = update published [] in
  assert (unchanged = []);
  let _, retained = update ~closed:outer_scope published [] in
  assert (List.assoc inner retained = errors)
