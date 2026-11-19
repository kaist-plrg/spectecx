type t

type analysis = private {
  sources : Sources.t;
  diagnostics : Spectec.Diagnostic.t list;
}

val create : unit -> t
val set : t -> path:string -> string -> t

(** Cache the analysis for the spec containing the file in the returned
    workspace. Documents in the same spec share one analysis. *)
val analyze : t -> string -> t * analysis

(** Recheck open specs containing the file, including overlapping spec roots. *)
val affected : t -> string -> t * analysis list

(** Remove the file's buffer. If no open documents remain in its spec, discard
    the cached analysis and return the spec directory. *)
val close : t -> string -> t * Sources.spec_directory option
