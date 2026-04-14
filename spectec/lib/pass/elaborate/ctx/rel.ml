open Lang
module Hint = Envs.Hint

type t = Il.nottyp * Hint.t * Il.rule list

let to_string (nottyp, inputs, rules) =
  Il.Print.string_of_nottyp nottyp
  ^ " " ^ Hint.to_string inputs ^ " =\n"
  ^ String.concat "\n   " (List.map Il.Print.string_of_rule rules)
