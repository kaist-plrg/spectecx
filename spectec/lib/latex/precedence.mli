(** The ranks follow the SpecTec parser. When comparisons, powers, prefix
    operators or sequences nest at the same rank, [needs_parens] requires
    parentheses to make the grouping explicit. For example, the parentheses in
    [-(-a)] make the second minus sign read as a sign on [a]. *)

(** [needs_parens] compares constructors in declaration order, from the lowest
    to the highest binding strength. *)
type rank =
  | Turnstile
  | Tilesturn
  | Step
  | Colon
  | Implication
  | Disjunction
  | Conjunction
  | Subtype
  | Assignment
  | Comparison
  | Cons
  | Arrow
  | Semicolon
  | Dot
  | Additive
  | Multiplicative
  | Prefix
  | Juxtaposition
  | Power
  | Postfix
  | Atomic

type side = Left_operand | Right_operand

val needs_parens : parent:rank -> side:side -> rank -> bool

(** A keyword, tag, operator text or bracket between operands takes the rank of
    a sequence item. *)
val of_atom : Xl.Atom.t -> rank

val of_binop : [ Xl.Bool.binop | Xl.Num.binop ] -> rank
val of_cmpop : [ Xl.Bool.cmpop | Xl.Num.cmpop ] -> rank
