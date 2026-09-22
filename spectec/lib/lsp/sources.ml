type scope = { directory : string; recursive : bool }

type t = {
  scope : scope;
  files : string list;
  contents : Spectec.spec_source list;
  errors : (string * string) list;
}

let canonical path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  try Unix.realpath path
  with Unix.Unix_error _ -> (
    try
      Filename.concat
        (Unix.realpath (Filename.dirname path))
        (Filename.basename path)
    with Unix.Unix_error _ -> path)

let scope_of path =
  let path = canonical path in
  match Spectec.spec_root_of_file path with
  | Some directory -> { directory = canonical directory; recursive = true }
  | None -> { directory = Filename.dirname path; recursive = false }

let contains scope path =
  let rec within directory =
    String.equal directory scope.directory
    || scope.recursive
       &&
       let parent = Filename.dirname directory in
       (not (String.equal parent directory)) && within parent
  in
  within (Filename.dirname (canonical path))

let load ~buffers ~path =
  let path = canonical path in
  let scope = scope_of path in
  let buffers =
    List.map
      (fun (source : Spectec.spec_source) ->
        { source with filename = canonical source.filename })
      buffers
  in
  let disk_files, discovery_errors =
    try
      let collect =
        if scope.recursive then Spectec.collect_spec_files
        else Spectec.collect_spec_siblings
      in
      (List.map canonical (collect scope.directory), [])
    with Sys_error message -> ([], [ (path, message) ])
  in
  let open_files =
    List.filter_map
      (fun (source : Spectec.spec_source) ->
        if
          Filename.check_suffix source.filename ".spectec"
          && contains scope source.filename
        then Some source.filename
        else None)
      buffers
  in
  let files =
    List.sort_uniq Spectec.compare_spec_files ((path :: disk_files) @ open_files)
  in
  let read filename =
    match
      List.find_opt
        (fun (source : Spectec.spec_source) ->
          String.equal filename source.filename)
        buffers
    with
    | Some source -> Either.Right source
    | None -> (
        match In_channel.with_open_bin filename In_channel.input_all with
        | contents -> Either.Right Spectec.{ filename; contents }
        | exception Sys_error message -> Either.Left (filename, message))
  in
  let errors, contents = List.partition_map read files in
  { scope; files; contents; errors = discovery_errors @ errors }

let equal a b =
  a.scope = b.scope && a.files = b.files && a.contents = b.contents
  && a.errors = b.errors

let text sources filename =
  List.find_map
    (fun (source : Spectec.spec_source) ->
      if String.equal source.filename filename then Some source.contents
      else None)
    sources.contents
