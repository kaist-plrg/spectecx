(** Output destination — stdout or a file. Files open lazily on first write;
    formatters auto-flush on newline. *)

type t =
  | Stdout
  | File of { path : string; mutable channel : out_channel option }

val stdout : t
val file : string -> t

(** Opens the underlying file on first call for a [File] destination. *)
val formatter : t -> Format.formatter

(** Flushes and closes an open file; no-op for [Stdout] or an unopened file. *)
val close : t -> unit
