open Lang.Il

(* Relation *)

type t = reltyp * rule list

let to_string (reltyp, rules) =
  "rel "
  ^ Print.string_of_reltyp reltyp
  ^ "\n"
  ^ String.concat "\n   " (List.map Print.string_of_rule rules)
