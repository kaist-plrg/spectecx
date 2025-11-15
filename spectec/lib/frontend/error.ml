open Util.Source

exception ParseError of region * string

(* Parser errors *)

let error (at : region) (msg : string) = raise (ParseError (at, msg))
let error_no_region (msg : string) = raise (ParseError (no_region, msg))
