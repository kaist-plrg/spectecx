open Lang.Sl
open Lang.Sl.Print

(* Relation *)

type t = Envs.Hint.t * exp list * instr list

let to_string (inputs, exps, instrs) =
  Envs.Hint.to_string inputs ^ string_of_exps ", " exps ^ "\n\n"
  ^ string_of_instrs instrs
