type spec_directory = { path : string; recursive : bool }

type t = private {
  spec_directory : spec_directory;
  files : string list;
  contents : Spectec.spec_source list;
  io_errors : (string * string) list;
}

(** [canonical_path path] is [path] made absolute with symbolic links resolved.
    When [path] cannot be resolved, only its directory is resolved. When neither
    can be resolved, the result is [path] made absolute. *)
val canonical_path : string -> string

val spec_directory_of : string -> spec_directory

(** [contains spec_directory ~file] resolves [file] with {!canonical_path} and
    tests whether its directory is [spec_directory.path]. When
    [spec_directory.recursive] is true, directories nested inside
    [spec_directory.path] also match. The file need not exist or have a
    [.spectec] extension. *)
val contains : spec_directory -> file:string -> bool

(** [load ~buffers path] loads the spec containing the file [path]. A file's
    text comes from [buffers] when [buffers] holds it, and from disk otherwise.
    A [.spectec] file in [buffers] belongs to the spec when the spec directory
    {!contains} it, even when the file does not exist on disk. *)
val load : buffers:Spectec.spec_source list -> string -> t

val equal : t -> t -> bool

(** [find_text_opt contents ~file] is the text of the source in [contents] whose
    filename is exactly [file]. *)
val find_text_opt : Spectec.spec_source list -> file:string -> string option
