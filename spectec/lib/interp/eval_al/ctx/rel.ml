open Lang.Al
module Il = Lang.Il

(* Relation *)

type t = reltyp * rule list

let to_string (reltyp, rules) =
  "rel "
  ^ Il.Print.string_of_reltyp reltyp
  ^ "\n"
  ^ String.concat "\n   " (List.map Print.string_of_rule rules)
