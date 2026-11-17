(** The one connection to a DAP client, and how it counts lines and columns. *)

module Location = Common.Location

type t = {
  conn : Transport.t;
  mutable line_base : int;  (** 1 unless the client counts lines from 0 *)
  mutable column_base : int;  (** 1 unless the client counts columns from 0 *)
}

(** Moves the run's output off stdout to stderr, and connects to the client over
    stdin and the original stdout. *)
val connect : unit -> unit

(** The session {!connect} opened. *)
val current : unit -> t

(** The run has a single thread, with this id. *)
val thread_id : int

(** [pos]'s line and column as the client counts them. The engine counts lines
    from 1 and columns from 0. *)
val client_line : t -> Location.pos -> Yojson.Safe.t

val client_column : t -> Location.pos -> Yojson.Safe.t

(** A line from the client, counted from 1 as the engine does. *)
val engine_line : t -> int -> int
