open Il

(* Relation *)

type t = Runtime_static.Rel.Hint.t * rule list

let to_string (inputs, rules) =
  "rel "
  ^ Runtime_static.Rel.Hint.to_string inputs
  ^ "\n"
  ^ String.concat "\n   " (List.map Print.string_of_rule rules)
