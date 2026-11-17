(** Reading the arguments of a DAP request. *)

(** A request the adapter refuses, with a message for the user. *)
exception Bad_request of string

(** Raises {!Bad_request} with the formatted message. *)
val bad_request : ('a, Format.formatter, unit, 'b) format4 -> 'a

(** The argument [name], or [`Null] when absent. *)
val member : string -> Transport.request -> Yojson.Safe.t

(** The integer argument [name], or [default] when absent. Raises {!Bad_request}
    when it is not an integer. *)
val int_or : string -> default:int -> Transport.request -> int

(** The boolean argument [name], or [default] when absent. Raises {!Bad_request}
    when it is not a boolean. *)
val bool_or : string -> default:bool -> Transport.request -> bool
