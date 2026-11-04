type spec_directory = { path : string; recursive : bool }

(** A failure while reading a file or listing a directory. *)
type io_error

(** [files] lists a spec's files in spec order, the order that
    {!Spectec.compare_spec_files} gives and the parser reads them in. [contents]
    holds the text of each readable file in that order. [io_errors] holds an
    error for each file that could not be read. When a directory of the spec
    cannot be listed, [io_errors] also holds one error at the loaded path. Every
    path in [t] is canonical. *)
type t = private {
  spec_directory : spec_directory;
  files : string list;
  contents : Spectec.spec_source list;
  io_errors : io_error list;
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

(** [io_diagnostics sources] returns diagnostics for [sources.io_errors] with
    canonical paths in their locations. Messages for read errors use the file's
    basename. Messages for listing errors name the directory relative to the
    parent of [sources.spec_directory.path]. *)
val io_diagnostics : t -> Spectec.Diagnostic.t list

val equal : t -> t -> bool

(** [find_text_opt contents ~file] is the text of the source in [contents] whose
    filename is exactly [file]. *)
val find_text_opt : Spectec.spec_source list -> file:string -> string option
