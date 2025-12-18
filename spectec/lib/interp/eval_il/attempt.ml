include Common.Attempt
open Common.Source

(* Converting failtraces to a single error *)
let error_with_failtraces (failtraces : failtrace list) =
  let sfailtrace =
    string_of_failtraces ~region_parent:no_region ~depth:0 failtraces
  in
  Error.error no_region ("tracing backtrack logs:\n" ^ sfailtrace)

(* Unwrap attempt or raise a fatal error with failtrace information *)
let unwrap_or_error (attempt : 'a attempt) : 'a =
  match attempt with Ok a -> a | Error traces -> error_with_failtraces traces

(* Operator for unwrapping at top-level *)
let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  f (unwrap_or_error attempt)
