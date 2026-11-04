module Spec_directories = Map.Make (struct
  type t = Sources.spec_directory

  let compare = compare
end)

type reports = (string * Linol_eio.Diagnostic.t list) list
type spec_reports = { files : string list; diagnostics : reports }
type t = spec_reports Spec_directories.t

let empty = Spec_directories.empty

let files_for spec_directory published =
  match Spec_directories.find_opt spec_directory published with
  | Some spec -> spec.files
  | None -> []

let update ~encoding ?closed published analyses =
  let removed =
    Option.fold ~none:[]
      ~some:(fun spec_directory -> files_for spec_directory published)
      closed
  in
  let published =
    Option.fold ~none:published
      ~some:(fun spec_directory ->
        Spec_directories.remove spec_directory published)
      closed
  in
  let published, files =
    List.fold_left
      (fun (published, files) (analysis : Workspace.analysis) ->
        let spec_directory = analysis.sources.spec_directory in
        let old = files_for spec_directory published in
        let diagnostics =
          Check.diagnostics ~encoding ~sources:analysis.sources
            analysis.diagnostics
        in
        let current =
          List.sort_uniq Spectec.compare_spec_files
            (analysis.sources.files @ List.map fst diagnostics)
        in
        ( Spec_directories.add spec_directory
            { files = current; diagnostics }
            published,
          old @ current @ files ))
      (published, removed) analyses
  in
  let reports =
    List.sort_uniq Spectec.compare_spec_files files
    |> List.map (fun file ->
           let diagnostics =
             Spec_directories.fold
               (fun _ spec all ->
                 Option.value (List.assoc_opt file spec.diagnostics) ~default:[]
                 @ all)
               published []
             |> List.sort_uniq compare
           in
           (file, diagnostics))
  in
  (published, reports)
