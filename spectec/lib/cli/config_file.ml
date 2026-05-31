type t = { spec_source : Spec_source.t option; batch_dir : string option }

let empty = { spec_source = None; batch_dir = None }
let default_filename = "spectecx.config"

let strip_comment line =
  match String.index_opt line '#' with
  | Some i -> String.sub line 0 i
  | None -> line

type raw = {
  spec_files : string list;
  spec_dir : string option;
  batch_dir : string option;
}

let empty_raw = { spec_files = []; spec_dir = None; batch_dir = None }

let field_for_target ~target key =
  match String.index_opt key '.' with
  | Some i when String.sub key 0 i = target ->
      Some (String.sub key (i + 1) (String.length key - i - 1))
  | _ -> None

let add_entry ~target acc key value =
  match field_for_target ~target key with
  | Some "spec" ->
      let files = String.split_on_char ' ' value |> List.filter (( <> ) "") in
      { acc with spec_files = acc.spec_files @ files }
  | Some "spec_dir" -> { acc with spec_dir = Some value }
  | Some "batch_dir" -> { acc with batch_dir = Some value }
  | _ -> acc

let finalize ~target { spec_files; spec_dir; batch_dir } =
  match (spec_files, spec_dir) with
  | _ :: _, Some _ ->
      Error
        (Spectec.Error.ConfigError
           ( Common.Source.no_region,
             Printf.sprintf
               "%s sets both '%s.spec' and '%s.spec_dir'; use one or the other"
               default_filename target target ))
  | [], None -> Ok { spec_source = None; batch_dir }
  | files, None ->
      Ok { spec_source = Some (Spec_source.Files files); batch_dir }
  | [], Some dir -> Ok { spec_source = Some (Spec_source.Dir dir); batch_dir }

let parse ~target contents =
  String.split_on_char '\n' contents
  |> List.fold_left
       (fun acc line ->
         let line = String.trim (strip_comment line) in
         match String.index_opt line '=' with
         | None -> acc
         | Some i ->
             let key = String.trim (String.sub line 0 i) in
             let value =
               String.trim
                 (String.sub line (i + 1) (String.length line - i - 1))
             in
             if key = "" || value = "" then acc
             else add_entry ~target acc key value)
       empty_raw
  |> finalize ~target

let load ~target ?(filename = default_filename) () =
  if Sys.file_exists filename then
    parse ~target (In_channel.with_open_text filename In_channel.input_all)
  else Ok empty
