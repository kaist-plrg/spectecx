include Common.Attempt

(* Unwrap attempt or raise a fatal error with failtrace information *)
let unwrap_or_error (attempt : 'a attempt) : 'a =
  match attempt with
  | Ok a -> a
  | Error traces -> Error.error_with_traces traces

(* Operator for unwrapping at top-level *)
let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  f (unwrap_or_error attempt)
