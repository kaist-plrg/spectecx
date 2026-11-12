module Atom = Xl.Atom

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

type assoc = Atom.assoc = Left | Right | Non
type side = Left_operand | Right_operand

let assoc_of_rank : rank -> assoc = function
  | Turnstile | Tilesturn | Comparison | Juxtaposition | Power | Prefix -> Non
  | Step | Implication | Assignment | Cons | Arrow -> Right
  | Colon | Disjunction | Conjunction | Subtype | Semicolon | Dot | Additive
  | Multiplicative | Postfix | Atomic ->
      Left

let needs_parens ~(parent : rank) ~(side : side) (child : rank) : bool =
  let order = Stdlib.compare child parent in
  order < 0
  || order = 0
     &&
     match (assoc_of_rank parent, side) with
     | Left, Right_operand | Right, Left_operand | Non, _ -> true
     | Left, Left_operand | Right, Right_operand -> false

(* Follow the parser so assignments with [:=] group inside [=>_] and [==>].
   [Xl.Atom.kind] assigns all three the same rank. *)
let of_atom (atom : Atom.t) : rank =
  match atom with
  | Turnstile -> Turnstile
  | Tilesturn -> Tilesturn
  | SqArrow | SqArrowStar -> Step
  | Colon | Tilde2 -> Colon
  | DoubleArrowSub | DoubleArrowLong -> Implication
  | Sub | Sup -> Subtype
  | ColonEq -> Assignment
  | Arrow | ArrowSub -> Arrow
  | Semicolon -> Semicolon
  | Dot | Dot2 | Dot3 -> Dot
  | Backslash -> Multiplicative
  | Keyword _ | Tag _ | Operator _ | LAngle | RAngle | LParen | RParen | LBrack
  | RBrack | LBrace | RBrace ->
      Juxtaposition

let of_binop (binop : [ Xl.Bool.binop | Xl.Num.binop ]) : rank =
  match binop with
  | `ImplOp | `EquivOp -> Implication
  | `OrOp -> Disjunction
  | `AndOp -> Conjunction
  | `AddOp | `SubOp -> Additive
  | `MulOp | `DivOp | `ModOp -> Multiplicative
  | `PowOp -> Power

let of_cmpop (_ : [ Xl.Bool.cmpop | Xl.Num.cmpop ]) : rank = Comparison
