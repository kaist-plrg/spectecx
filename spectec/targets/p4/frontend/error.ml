module Location = Common.Location

exception P4ParseError of Location.t * string

(* P4 Parsing errors *)

let error (at : Location.t) (msg : string) = raise (P4ParseError (at, msg))

let error_no_location (msg : string) =
  raise (P4ParseError (Location.Unknown, msg))
