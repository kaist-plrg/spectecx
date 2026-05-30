include Common.Attempt

let error_with_failtraces (failtraces : failtrace list) =
  raise (Error.BacktrackError failtraces)

let unwrap_or_error (attempt : 'a attempt) : 'a =
  match attempt with Ok a -> a | Error traces -> error_with_failtraces traces

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  f (unwrap_or_error attempt)
