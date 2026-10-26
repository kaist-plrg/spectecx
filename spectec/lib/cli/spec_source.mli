(** Where a target's spec files come from: an explicit list of files, or a
    directory to collect. *)

type t = Files of string list | Dir of string

(** [resolve ~from_cli ~from_config_file ~default_dir] is the first source
    present among [from_cli], [from_config_file], and [Dir default_dir].
    [from_config_file] is normally the [spec_source] field of the
    {!Config_file.t} that {!Config_file.load} returns. *)
val resolve :
  from_cli:t option -> from_config_file:t option -> default_dir:string -> t

(** [files src] is the spec files for [src]: a {!Files} list as given, or the
    [.spectec] files under a {!Dir} via {!Spectec.collect_spec_files}. Errors
    when the directory is missing or is not a directory. *)
val files : t -> (string list, Spectec.Error.t) result
