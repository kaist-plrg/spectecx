type analysis = { sources : Sources.t; diagnostics : Spectec.Diagnostic.t list }

module Paths = Map.Make (String)

module Scopes = Map.Make (struct
  type t = Sources.scope

  let compare = compare
end)

type t = { buffers : string Paths.t; analyses : analysis Scopes.t }

let create () = { buffers = Paths.empty; analyses = Scopes.empty }

let set workspace ~path text =
  {
    workspace with
    buffers = Paths.add (Sources.canonical path) text workspace.buffers;
  }

let analyze workspace path =
  let buffers =
    Paths.fold
      (fun filename contents acc -> Spectec.{ filename; contents } :: acc)
      workspace.buffers []
  in
  let sources = Sources.load ~buffers ~path in
  let previous = Scopes.find_opt sources.scope workspace.analyses in
  match previous with
  | Some previous when Sources.equal sources previous.sources ->
      (workspace, previous)
  | _ ->
      let diagnostics = Check.analyze sources in
      let analysis = { sources; diagnostics } in
      ( {
          workspace with
          analyses = Scopes.add sources.scope analysis workspace.analyses;
        },
        analysis )

let affected workspace path =
  let path = Sources.canonical path in
  let scopes =
    Paths.fold
      (fun file _ scopes ->
        let scope = Sources.scope_of file in
        let includes =
          Sources.contains scope path
          ||
          match Scopes.find_opt scope workspace.analyses with
          | Some analysis -> List.mem path analysis.sources.files
          | None -> false
        in
        if includes && not (List.mem_assoc scope scopes) then
          (scope, file) :: scopes
        else scopes)
      workspace.buffers []
    |> List.sort (fun (_, a) (_, b) -> Spectec.compare_spec_files a b)
  in
  let workspace, analyses =
    List.fold_left
      (fun (workspace, analyses) (_, file) ->
        let workspace, analysis = analyze workspace file in
        (workspace, analysis :: analyses))
      (workspace, []) scopes
  in
  (workspace, List.rev analyses)

let close workspace path =
  let path = Sources.canonical path in
  let scope = Sources.scope_of path in
  let workspace =
    { workspace with buffers = Paths.remove path workspace.buffers }
  in
  if
    Paths.exists (fun file _ -> Sources.scope_of file = scope) workspace.buffers
  then (workspace, None)
  else
    ( { workspace with analyses = Scopes.remove scope workspace.analyses },
      Some scope )
