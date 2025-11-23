open Lang.Il

(* Relation *)

type t = Static.Rel.Hint.t * rule list

let to_string (inputs, rules) =
  "rel "
  ^ Static.Rel.Hint.to_string inputs
  ^ "\n"
  ^ String.concat "\n   " (List.map Print.string_of_rule rules)
