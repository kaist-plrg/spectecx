open Common.Source

type error = region * string
type 'a result = ('a, error) Stdlib.result

exception ParseError of error

(* Parser errors *)

let error (at : region) (msg : string) = raise (ParseError (at, msg))
let error_no_region (msg : string) = raise (ParseError (no_region, msg))
let to_string ((at, msg) : error) = Common.Error.string_of_located_error at msg

let to_diagnostic ((at, msg) : error) : Diag.t =
  Diag.error ~source:"parse" at msg
