(** Debug Adapter Protocol transport: JSON messages framed by a [Content-Length]
    header, read from and written to a pair of channels.

    The client sends requests; the adapter answers each with a response and
    sends events on its own. Writing is not thread-safe: one thread does all of
    it. *)

(** Malformed input, with what was wrong with it. *)
exception Protocol_error of string

type request = {
  seq : int;
  command : string;
  arguments : Yojson.Safe.t;  (** [`Null] when the request has none *)
}

(** A connection to one client. *)
type t

val create : in_channel -> out_channel -> t

(** Reads the next request, or [None] when the input ends between messages.
    Raises {!Protocol_error} on a malformed message or one that is not a
    request. *)
val read_request : t -> request option

(** Answers [request] successfully, with [body] if given. *)
val respond : t -> ?body:Yojson.Safe.t -> request -> unit

(** Answers [request] with a failure and a message for the user. *)
val respond_error : t -> request -> string -> unit

(** Sends the event [name], with [body] if given. *)
val send_event : t -> ?body:Yojson.Safe.t -> string -> unit
