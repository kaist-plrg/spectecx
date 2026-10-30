(** Parser errors. *)

module Location = Common.Location

exception ImptyParseError of Location.t * string

let error (at : Location.t) (msg : string) = raise (ImptyParseError (at, msg))
