(** Project-local CLI defaults from a [spectecx.config] file in the current
    directory; an explicit flag always takes precedence over them.

    Keys are namespaced by target. The file is [key = value] lines, with [#]
    comments and blank lines ignored. For a target [t]:
    - [t.spec]: whitespace-separated spec files;
    - [t.spec_dir]: a directory of [.spectec] files, collected recursively;
    - [t.batch_dir]: the default input directory for the [batch] command.

    [t.spec] and [t.spec_dir] are mutually exclusive. *)

type t = { spec_source : Spec_source.t option; batch_dir : string option }

(** Defaults with nothing configured. *)
val empty : t

(** [load ~target ()] returns the defaults configured for [target], {!empty}
    when the file is absent, or a [ConfigError] when [target] sets both [spec]
    and [spec_dir]. *)
val load :
  target:string -> ?filename:string -> unit -> (t, Spectec.Error.t) result
