open Util.Source
open Il

type t =
  | TypeError of region * string * value
  | ArityError of region * string
  | RuntimeError of region * string

let runtime (r : region) (msg : string) : t = RuntimeError (r, msg)
let arity (r : region) (msg : string) : t = ArityError (r, msg)
let type_err (r : region) (msg : string) (v : value) : t = TypeError (r, msg, v)
