open Lang.Sl
open Lang.Sl.Print

(* Relation *)

type t = Semantics.Hint.t * exp list * instr list

let to_string (inputs, exps, instrs) =
  Semantics.Hint.to_string inputs
  ^ string_of_exps ", " exps ^ "\n\n" ^ string_of_instrs instrs
