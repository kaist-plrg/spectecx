open Lang.Il

(* Relation *)

type t = Hints.Input.t * rule list

let to_string (inputs, rules) =
  "rel "
  ^ Hints.Input.to_string inputs
  ^ "\n"
  ^ String.concat "\n   " (List.map Print.string_of_rule rules)
