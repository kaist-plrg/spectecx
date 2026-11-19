type analysis = { sources : Sources.t; diagnostics : Spectec.Diagnostic.t list }

module Paths = Map.Make (String)

module Spec_directories = Map.Make (struct
  type t = Sources.spec_directory

  let compare = compare
end)

type t = { buffers : string Paths.t; analyses : analysis Spec_directories.t }

let create () = { buffers = Paths.empty; analyses = Spec_directories.empty }

let set workspace ~path text =
  {
    workspace with
    buffers = Paths.add (Sources.canonical_path path) text workspace.buffers;
  }

let analyze workspace path =
  let buffers =
    Paths.fold
      (fun filename contents acc -> Spectec.{ filename; contents } :: acc)
      workspace.buffers []
  in
  let sources = Sources.load ~buffers path in
  let previous =
    Spec_directories.find_opt sources.spec_directory workspace.analyses
  in
  match previous with
  | Some previous when Sources.equal sources previous.sources ->
      (workspace, previous)
  | _ ->
      let diagnostics = Check.analyze sources in
      let analysis = { sources; diagnostics } in
      ( {
          workspace with
          analyses =
            Spec_directories.add sources.spec_directory analysis
              workspace.analyses;
        },
        analysis )

let affected workspace path =
  let path = Sources.canonical_path path in
  let spec_directories =
    Paths.fold
      (fun file _ spec_directories ->
        let spec_directory = Sources.spec_directory_of file in
        let includes =
          Sources.contains spec_directory ~file:path
          ||
          match Spec_directories.find_opt spec_directory workspace.analyses with
          | Some analysis -> List.mem path analysis.sources.files
          | None -> false
        in
        if includes && not (List.mem_assoc spec_directory spec_directories) then
          (spec_directory, file) :: spec_directories
        else spec_directories)
      workspace.buffers []
    |> List.sort (fun (_, a) (_, b) -> Spectec.compare_spec_files a b)
  in
  let workspace, analyses =
    List.fold_left
      (fun (workspace, analyses) (_, file) ->
        let workspace, analysis = analyze workspace file in
        (workspace, analysis :: analyses))
      (workspace, []) spec_directories
  in
  (workspace, List.rev analyses)

let close workspace path =
  let path = Sources.canonical_path path in
  let spec_directory = Sources.spec_directory_of path in
  let workspace =
    { workspace with buffers = Paths.remove path workspace.buffers }
  in
  if
    Paths.exists
      (fun file _ -> Sources.spec_directory_of file = spec_directory)
      workspace.buffers
  then (workspace, None)
  else
    ( {
        workspace with
        analyses = Spec_directories.remove spec_directory workspace.analyses;
      },
      Some spec_directory )
