open Common.Source
open Lang.Il

type t =
  | TypeError of region * string * value
  | ArityError of region * string
  | RuntimeError of region * string
  | MissingImplError of region * string

type 'a result = ('a, t) Stdlib.result

let runtime (r : region) (msg : string) : t = RuntimeError (r, msg)
let arity (r : region) (msg : string) : t = ArityError (r, msg)
let type_err (r : region) (msg : string) (v : value) : t = TypeError (r, msg, v)
let missing_impl (r : region) (msg : string) : t = MissingImplError (r, msg)

let string_of_error = function
  | TypeError (at, expected, got) ->
      Printf.sprintf "%sType error: expected %s, got %s" (string_of_region at)
        expected (Value.to_string got)
  | ArityError (at, msg) ->
      Printf.sprintf "%sArity error: %s" (string_of_region at) msg
  | RuntimeError (at, msg) ->
      Printf.sprintf "%sRuntime error: %s" (string_of_region at) msg
  | MissingImplError (at, msg) ->
      Printf.sprintf "%sMissing builtin implementation: %s"
        (string_of_region at) msg
