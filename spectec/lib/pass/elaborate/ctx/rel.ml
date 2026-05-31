open Lang

type t = Il.reltyp * Il.rule list

let to_string (reltyp, rules) =
  Il.Print.string_of_reltyp reltyp
  ^ " =\n"
  ^ String.concat "\n   " (List.map Il.Print.string_of_rule rules)
