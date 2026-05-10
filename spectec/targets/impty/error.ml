(** Parser errors. *)

open Common.Source

exception ImptyParseError of region * string

let error (at : region) (msg : string) = raise (ImptyParseError (at, msg))
