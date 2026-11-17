(** The requests that configure a session before and between stops: [initialize]
    and breakpoints. Each returns the response body, and raises
    {!Request.Bad_request} on bad arguments. *)

(** The body of the [initialize] response. *)
val capabilities : Yojson.Safe.t

(** Records how the client counts lines and columns. *)
val initialize : Session.t -> Transport.request -> unit

(** Replaces every relation and rule breakpoint with those named, answering for
    each name in order. *)
val set_function_breakpoints : Transport.request -> Yojson.Safe.t

(** Replaces the premise breakpoints in one source with those on the given
    lines, answering for each line in order. A line between premises moves to
    the next one, and the answer says where it landed. *)
val set_breakpoints : Session.t -> Transport.request -> Yojson.Safe.t
