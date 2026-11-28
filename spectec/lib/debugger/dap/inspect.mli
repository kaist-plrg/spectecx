(** The requests that look at a stopped run: its stack, the variables of a
    frame, and the values in them. Each returns the response body, and raises
    {!Request.Bad_request} on bad arguments. *)

val stack_trace : Session.t -> Transport.request -> Yojson.Safe.t
val scopes : Transport.request -> Yojson.Safe.t
val variables : Transport.request -> Yojson.Safe.t

(** Evaluates a variable of the frame: [x*] names that binding, and [x] names
    plain [x] or, failing that, the one binding of [x] with iterations. *)
val evaluate : Transport.request -> Yojson.Safe.t

(** Forgets every variables reference handed out. *)
val reset : unit -> unit
