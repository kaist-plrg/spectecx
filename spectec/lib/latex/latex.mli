(** Render expressions, types and premises in SpecTec's external language (EL)
    as LaTeX math. Operator precedence and the spacing of relation symbols
    determine the parentheses the output needs, independently of the author's
    [( )] and [$( )].

    SpecTec reserves forms such as [%0] and [%latex("x")] for [hint(...)]. The
    functions [of_exp], [of_typ] and [of_prem] raise [Invalid_argument] if they
    encounter one of these forms. *)

(** A formula with its symbols, fonts and grouping chosen. *)
type t

val of_exp : El.exp -> t
val of_typ : El.typ -> t

(** Renders a rule premise in the relation's notation and omits the relation
    name. Negated rule premises have a leading negation sign. *)
val of_prem : El.prem -> t

(** Writes one line of LaTeX math for the caller to place in math mode. LaTeX
    requires the [amsfonts] package and support for any non-ASCII characters in
    text literals. MathJax requires the [textmacros] extension. *)
val to_string : t -> string
