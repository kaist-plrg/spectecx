(** A formula fixes the symbols, fonts and grouping of an expression, type or
    premise. It occupies one line. *)

type font =
  | Roman  (** Upright serif, set with [\mathrm]. *)
  | Sans  (** Upright sans-serif, set with [\mathsf]. *)
  | Mono  (** Typewriter, set with [\mathtt]. *)
  | Blackboard  (** Double-struck, set with [\mathbb]. *)

type delimiter = Paren | Bracket | Brace | Angle | Bar

type symbol =
  | Equal
  | Not_equal
  | Less
  | Greater
  | Less_equal
  | Greater_equal
  | Plus
  | Minus
  | Cdot
  | Slash
  | Mod
  | Neg
  | Wedge
  | Vee
  | Rightarrow
  | Leftrightarrow
  | Longrightarrow
  | Hookrightarrow
  | Hookrightarrow_star
  | To
  | Vdash
  | Dashv
  | In
  | Double_colon
  | Double_plus
  | Less_colon
  | Colon_greater
  | Colon
  | Colon_equal
  | Semicolon
  | Dot
  | Two_dots
  | Ellipsis
  | Sim
  | Setminus
  | Ast
  | Question
  | Epsilon
  | Left_angle
  | Right_angle
  | Left_paren
  | Right_paren
  | Left_bracket
  | Right_bracket
  | Left_brace
  | Right_brace
  | Operator of string
      (** An operator of the language being specified. Typewriter text preserves
          its spelling and spacing. *)

type t =
  | Word of font * string  (** A name, keyword or literal set in one font. *)
  | Numeral of string  (** Decimal digits. *)
  | Text of string
      (** Typewriter text with every space preserved. [to_string] writes
          backslashes, carets, tildes and braces as math symbols between runs of
          text. *)
  | Symbol of symbol
  | Concat of t list  (** Parts joined without a separator. *)
  | Seq of t list  (** Items separated by a thin space. *)
  | Infix of { left : t; op : t; right : t }
  | Commas of t list
  | Delimited of delimiter * t
  | Script of { base : t; sub : t option; sup : t option }

(** Writes any formula as LaTeX math. *)
val to_string : t -> string
