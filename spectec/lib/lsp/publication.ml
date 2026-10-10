module Scopes = Map.Make (struct
  type t = Sources.scope

  let compare = compare
end)

type reports = (string * Linol_eio.Diagnostic.t list) list
type spec_reports = { files : string list; diagnostics : reports }
type t = spec_reports Scopes.t

let empty = Scopes.empty

let files_for scope published =
  match Scopes.find_opt scope published with
  | Some spec -> spec.files
  | None -> []

let update ~encoding ?closed published analyses =
  let removed =
    Option.fold ~none:[] ~some:(fun scope -> files_for scope published) closed
  in
  let published =
    Option.fold ~none:published
      ~some:(fun scope -> Scopes.remove scope published)
      closed
  in
  let published, files =
    List.fold_left
      (fun (published, files) (analysis : Workspace.analysis) ->
        let scope = analysis.sources.scope in
        let old = files_for scope published in
        let diagnostics =
          Check.diagnostics ~encoding ~sources:analysis.sources
            analysis.diagnostics
        in
        let current =
          List.sort_uniq Spectec.compare_spec_files
            (analysis.sources.files @ List.map fst diagnostics)
        in
        ( Scopes.add scope { files = current; diagnostics } published,
          old @ current @ files ))
      (published, removed) analyses
  in
  let reports =
    List.sort_uniq Spectec.compare_spec_files files
    |> List.map (fun file ->
           let diagnostics =
             Scopes.fold
               (fun _ spec all ->
                 Option.value (List.assoc_opt file spec.diagnostics) ~default:[]
                 @ all)
               published []
             |> List.sort_uniq compare
           in
           (file, diagnostics))
  in
  (published, reports)
