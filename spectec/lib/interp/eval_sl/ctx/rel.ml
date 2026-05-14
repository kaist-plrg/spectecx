open Lang.Sl
open Lang.Sl.Print

(* Relation *)

type t = Hints.Input.t * exp list * block * elseblock option

let to_string (inputs, exps, block, elseblock_opt) =
  Hints.Input.to_string inputs
  ^ string_of_exps ", " exps ^ "\n\n" ^ string_of_block block
  ^
  match elseblock_opt with
  | None -> ""
  | Some elseblock -> "\n\notherwise\n\n" ^ string_of_instrs elseblock
