module Location = Common.Location
open Lang.Il

type t =
  | TypeError of Location.t * string * value
  | ArityError of Location.t * string
  | RuntimeError of Location.t * string
  | MissingImplError of Location.t * string

type 'a result = ('a, t) Stdlib.result

let runtime (r : Location.t) (msg : string) : t = RuntimeError (r, msg)
let arity (r : Location.t) (msg : string) : t = ArityError (r, msg)

let type_err (r : Location.t) (msg : string) (v : value) : t =
  TypeError (r, msg, v)

let missing_impl (r : Location.t) (msg : string) : t = MissingImplError (r, msg)

let location_and_message = function
  | TypeError (at, expected, got) ->
      ( at,
        Printf.sprintf "Type error: expected %s, got %s" expected
          (Value.to_string got) )
  | ArityError (at, msg) -> (at, Printf.sprintf "Arity error: %s" msg)
  | RuntimeError (at, msg) -> (at, Printf.sprintf "Runtime error: %s" msg)
  | MissingImplError (at, msg) ->
      (at, Printf.sprintf "Missing builtin implementation: %s" msg)

let to_diagnostic (err : t) : Diag.t =
  let at, message = location_and_message err in
  Diag.error ~source:"builtins" at message
