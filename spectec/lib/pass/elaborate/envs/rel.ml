open Lang

(* Input hints for rules *)

module Hint = Semantics.Hint

(* Relation *)

type t = El.nottyp * Hint.t * Il.rule list

let to_string (nottyp, inputs, rules) =
  El.Print.string_of_nottyp nottyp
  ^ " " ^ Hint.to_string inputs ^ " =\n"
  ^ String.concat "\n   " (List.map Il.Print.string_of_rule rules)
