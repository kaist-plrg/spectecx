(** Choose the symbols, fonts and parentheses for EL expressions, types and
    premises. *)

open El

val formula_of_exp : exp -> Formula.t
val formula_of_typ : typ -> Formula.t
val formula_of_prem : prem -> Formula.t
