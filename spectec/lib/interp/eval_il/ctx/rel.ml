open Lang.Il

(* Relation *)

type t = Envs.Hint.t * rule list

let to_string (inputs, rules) =
  "rel " ^ Envs.Hint.to_string inputs ^ "\n"
  ^ String.concat "\n   " (List.map Print.string_of_rule rules)
