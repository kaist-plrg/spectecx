type t = Files of string list | Dir of string

let resolve ~from_cli ~from_config_file ~default_dir =
  match from_cli with
  | Some source -> source
  | None -> Option.value from_config_file ~default:(Dir default_dir)

let files = function
  | Files files -> Ok files
  | Dir dir ->
      if Sys.file_exists dir && Sys.is_directory dir then
        Ok (Spectec.collect_spec_files dir)
      else
        Error
          (Spectec.Error.DirectoryError
             (Printf.sprintf
                "spec directory %s does not exist; pass --spec or --spec-dir"
                dir))
