open Lang.Il
open Builtins

(* dec $max_int(int* ) : int *)

let max ~at (nums : Bigint.t list) : Value.t result =
  match nums with
  | [] -> Error (Error.runtime at "max_int: the list is empty")
  | num :: nums -> Ok (Value.int (List.fold_left Bigint.max num nums))

let builtins = [ ("max_int", Define.T0.a1 (Arg.list_of Arg.int) max) ]
