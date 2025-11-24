open Lang.Sl
open Lang.Sl.Print

(* Relation *)

type t = Static.Rel.Hint.t * exp list * instr list

let to_string (inputs, exps, instrs) =
  Static.Rel.Hint.to_string inputs
  ^ string_of_exps ", " exps ^ "\n\n" ^ string_of_instrs instrs
