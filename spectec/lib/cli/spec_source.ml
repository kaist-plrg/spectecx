type t = Files of string list | Dir of string

let collect_dir dir =
  let rec collect spec_files_rev dir =
    let entries = Sys.readdir dir in
    Array.sort String.compare entries;
    Array.fold_left
      (fun spec_files_rev entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then collect spec_files_rev path
        else if Filename.check_suffix entry ".spectec" then
          path :: spec_files_rev
        else spec_files_rev)
      spec_files_rev entries
  in
  collect [] dir |> List.rev

let files = function
  | Files files -> Ok files
  | Dir dir ->
      if Sys.file_exists dir && Sys.is_directory dir then Ok (collect_dir dir)
      else
        Error
          (Spectec.Error.DirectoryError
             (Printf.sprintf
                "spec directory %s does not exist; pass --spec or --spec-dir"
                dir))
