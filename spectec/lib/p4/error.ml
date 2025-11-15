open Util.Source

exception P4ParseError of region * string

(* P4 Parsing errors *)

let error (at : region) (msg : string) = raise (P4ParseError (at, msg))
let error_no_region (msg : string) = raise (P4ParseError (no_region, msg))
