type spec_directory = { path : string; recursive : bool }

type io_error =
  | UnreadableFile of { file : string; reason : string }
  | UnlistableDirectory of {
      loaded : string;
      directory : string;
      reason : string;
    }

type t = {
  spec_directory : spec_directory;
  files : string list;
  contents : Spectec.spec_source list;
  io_errors : io_error list;
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

let is_directory path = Sys.file_exists path && Sys.is_directory path

let spec_directory_of path =
  let path = canonical_path path in
  let directory = if is_directory path then path else Filename.dirname path in
  match Spectec.find_spec_root_opt directory with
  | Some root -> { path = canonical_path root; recursive = true }
  | None -> { path = directory; recursive = false }

let collect_canonical_files ~recursive directory =
  List.map canonical_path (Spectec.collect_spec_files ~recursive directory)

let relative_files_at path ~relative_to =
  let relative_to = canonical_path relative_to in
  let files =
    if is_directory path then
      collect_canonical_files ~recursive:(spec_directory_of path).recursive path
    else [ canonical_path path ]
  in
  List.map (fun file -> Core.Filename.of_absolute_exn file ~relative_to) files

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
      ( collect_canonical_files ~recursive:spec_directory.recursive
          spec_directory.path,
        [] )
    with Sys_error message ->
      let directory, reason = split_sys_error message in
      ( [],
        [
          UnlistableDirectory
            {
              loaded = path;
              directory =
                Option.fold directory ~none:spec_directory.path
                  ~some:canonical_path;
              reason;
            };
        ] )
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
  let selected_files = if is_directory path then [] else [ path ] in
  let files =
    List.sort_uniq Spectec.compare_spec_files
      (selected_files @ disk_files @ open_files)
  in
  let read filename =
    match find_text_opt buffers ~file:filename with
    | Some contents -> Either.Right Spectec.{ filename; contents }
    | None -> (
        match In_channel.with_open_bin filename In_channel.input_all with
        | contents -> Either.Right Spectec.{ filename; contents }
        | exception Sys_error message ->
            Either.Left
              (UnreadableFile
                 { file = filename; reason = snd (split_sys_error message) }))
  in
  let file_errors, contents = List.partition_map read files in
  { spec_directory; files; contents; io_errors = listing_errors @ file_errors }

let diagnostic_of_io_error error ~relative_to ~location_path_of =
  match error with
  | UnreadableFile { file; reason } ->
      Spectec.Diagnostic.error ~source:"io"
        (Common.Location.File (location_path_of file))
        (Printf.sprintf "cannot read %s: %s" (Filename.basename file) reason)
  | UnlistableDirectory { loaded; directory; reason } ->
      Spectec.Diagnostic.error ~source:"io"
        (Common.Location.File (location_path_of loaded))
        (Printf.sprintf "cannot list %s: %s"
           (Core.Filename.of_absolute_exn directory ~relative_to)
           reason)

let io_diagnostics sources =
  let relative_to = Filename.dirname sources.spec_directory.path in
  List.map
    (fun error ->
      diagnostic_of_io_error error ~relative_to ~location_path_of:Fun.id)
    sources.io_errors

let relative_contents sources ~relative_to =
  let relative_to = canonical_path relative_to in
  List.map
    (fun (source : Spectec.spec_source) ->
      {
        source with
        filename = Core.Filename.of_absolute_exn source.filename ~relative_to;
      })
    sources.contents

let relative_io_diagnostics sources ~relative_to =
  let relative_to = canonical_path relative_to in
  let location_path_of path = Core.Filename.of_absolute_exn path ~relative_to in
  List.map
    (fun error -> diagnostic_of_io_error error ~relative_to ~location_path_of)
    sources.io_errors

let equal a b =
  a.spec_directory = b.spec_directory
  && a.files = b.files && a.contents = b.contents && a.io_errors = b.io_errors
