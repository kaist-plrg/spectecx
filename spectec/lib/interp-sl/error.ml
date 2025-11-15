open Util.Error
open Util.Source

exception InterpError of region * string

(* Error *)

let error (at : region) (msg : string) = raise (InterpError (at, msg))
let warn (at : region) (msg : string) = warn at "sl-interp" msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg
