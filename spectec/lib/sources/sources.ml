type spec_directory = { path : string; recursive : bool }

type t = {
  spec_directory : spec_directory;
  files : string list;
  contents : Spectec.spec_source list;
  io_errors : (string * string) list;
}

let canonical_path path =
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

let spec_directory_of path =
  let path = canonical_path path in
  match Spectec.spec_root_of_file path with
  | Some directory -> { path = canonical_path directory; recursive = true }
  | None -> { path = Filename.dirname path; recursive = false }

let contains spec_directory ~file =
  let rec within directory =
    String.equal directory spec_directory.path
    || spec_directory.recursive
       &&
       let parent = Filename.dirname directory in
       (not (String.equal parent directory)) && within parent
  in
  within (Filename.dirname (canonical_path file))

(* The message of a [Sys_error] from opening or listing a path is the path,
   [": "] and a reason without a colon. The message of a [Sys_error] from
   reading an open file is the reason alone. *)
let split_sys_error message =
  match String.rindex_opt message ':' with
  | Some i ->
      ( Some (String.sub message 0 i),
        String.sub message (i + 2) (String.length message - i - 2) )
  | None -> (None, message)

let find_text_opt contents ~file =
  List.find_map
    (fun (source : Spectec.spec_source) ->
      if String.equal source.filename file then Some source.contents else None)
    contents

let load ~buffers path =
  let path = canonical_path path in
  let spec_directory = spec_directory_of path in
  let buffers =
    List.map
      (fun (source : Spectec.spec_source) ->
        { source with filename = canonical_path source.filename })
      buffers
  in
  let disk_files, listing_errors =
    try
      let collect =
        if spec_directory.recursive then Spectec.collect_spec_files
        else Spectec.collect_spec_siblings
      in
      (List.map canonical_path (collect spec_directory.path), [])
    with Sys_error message -> ([], [ (path, snd (split_sys_error message)) ])
  in
  let open_files =
    List.filter_map
      (fun (source : Spectec.spec_source) ->
        if
          Filename.check_suffix source.filename ".spectec"
          && contains spec_directory ~file:source.filename
        then Some source.filename
        else None)
      buffers
  in
  let files =
    List.sort_uniq Spectec.compare_spec_files ((path :: disk_files) @ open_files)
  in
  let read filename =
    match find_text_opt buffers ~file:filename with
    | Some contents -> Either.Right Spectec.{ filename; contents }
    | None -> (
        match In_channel.with_open_bin filename In_channel.input_all with
        | contents -> Either.Right Spectec.{ filename; contents }
        | exception Sys_error message ->
            Either.Left (filename, snd (split_sys_error message)))
  in
  let file_errors, contents = List.partition_map read files in
  { spec_directory; files; contents; io_errors = listing_errors @ file_errors }

let io_diagnostics sources =
  List.map
    (fun (file, reason) ->
      Spectec.Diagnostic.error ~source:"io" (Common.Location.File file)
        (Printf.sprintf "cannot read %s: %s" (Filename.basename file) reason))
    sources.io_errors

let equal a b =
  a.spec_directory = b.spec_directory
  && a.files = b.files && a.contents = b.contents && a.io_errors = b.io_errors
