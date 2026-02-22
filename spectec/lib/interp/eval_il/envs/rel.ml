open Lang.Il

(* Relation *)

type t = Semantics.Hint.t * rule list

let to_string (inputs, rules) =
  "rel "
  ^ Semantics.Hint.to_string inputs
  ^ "\n"
  ^ String.concat "\n   " (List.map Print.string_of_rule rules)
