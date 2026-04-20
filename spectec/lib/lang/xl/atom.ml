[@@@ocamlformat "disable"]

type t =
  | Atom of string       (* atomid *)
  | SilentAtom of string (* `atomid *)
  | Sub                  (* `<:` *)
  | Sup                  (* `:>` *)
  | Turnstile            (* `|-` *)
  | Tilesturn            (* `-|` *)
  | Tick                 (* ```` *)
  | DoubleQuote          (* ``''` *)
  | Underscore           (* ``_` *)
  | Arrow                (* `->` *)
  | TickArrow            (* escaped ``->` as a plain atom *)
  | ArrowSub             (* `->_` *)
  | DoubleArrow          (* ``=>` *)
  | DoubleArrowSub       (* ``=>_` *)
  | DoubleArrowLong      (* ``==>` *)
  | SqArrow              (* `~>` *)
  | SqArrowStar          (* `~>*` *)
  | Dot                  (* `.` *)
  | TickDot              (* ``.` *)
  | Dot2                 (* `..` *)
  | TickDot2             (* ``..` *)
  | Dot3                 (* `...` *)
  | TickDot3             (* ``...` *)
  | Comma                (* ``,` *)
  | Semicolon            (* `;` *)
  | TickSemicolon        (* ``;` *)
  | Colon                (* `:` *)
  | TickColon            (* ``:` *)
  | Hash                 (* ``#` *)
  | Dollar               (* ``$` *)
  | At                   (* ``@` *)
  | Quest                (* ``?` *)
  | Bang                 (* ``!` *)
  | BangEq               (* ``!=` *)
  | Tilde                (* ``~` *)
  | Tilde2               (* `~~` *)
  | LAngle               (* ``<` *)
  | TickLAngle           (* ```<` *)
  | LAngle2              (* `<<` *)
  | LAngleEq             (* ``<=` *)
  | LAngle2Eq            (* `<<=` *)
  | RAngle               (* ``>` *)
  | TickRAngle           (* ```>` *)
  | RAngle2              (* `>>` *)
  | RAngleEq             (* ``>=` *)
  | RAngle2Eq            (* `>>=` *)
  | LParen               (* ``(` *)
  | RParen               (* ``)` *)
  | LBrack               (* ``[` *)
  | TickLBrack           (* ```[` *)
  | RBrack               (* ``]` *)
  | TickRBrack           (* ```]` *)
  | LBrace               (* ``{` *)
  | TickLBrace           (* ```{` *)
  | LBraceHashRBrace     (* `{#}` *)
  | RBrace               (* ``}` *)
  | TickRBrace           (* ```}` *)
  | Plus                 (* ``+` *)
  | Plus2                (* ``++` *)
  | PlusEq               (* ``+=` *)
  | Minus                (* ``-` *)
  | MinusEq              (* ``-=` *)
  | Star                 (* ``*` *)
  | StarEq               (* ``*=` *)
  | Slash                (* ``/` *)
  | SlashEq              (* ``/=` *)
  | Backslash            (* ``\` *)
  | Percent              (* ``%` *)
  | PercentEq            (* ``%=` *)
  | Eq                   (* ``=` *)
  | Eq2                  (* `==` *)
  | Amp                  (* ``&` *)
  | Amp2                 (* ``&&` *)
  | Amp3                 (* ``&&&` *)
  | AmpEq                (* ``&=` *)
  | Up                   (* ``^` *)
  | UpEq                 (* ``^=` *)
  | Bar                  (* ``|` *)
  | Bar2                 (* ``||` *)
  | BarEq                (* ``|=` *)
  | SPlus                (* ``|+|` *)
  | SPlusEq              (* ``|+|=` *)
  | SMinus               (* ``|-|` *)
  | SMinusEq             (* ``|-|=` *)
[@@@ocamlformat "enable"]

let compare atom_a atom_b = compare atom_a atom_b
let eq atom_a atom_b = compare atom_a atom_b = 0

(* Precedence mirrors parser.mly: relop atoms (levels 1-4) are looser than
   infixop atoms (levels 5-9). Higher level = tighter. *)

type assoc = Left | Right | Non

type kind =
  | Plain
  | Infix of { assoc : assoc; level : int }
  | BracketL
  | BracketR

let kind : t -> kind = function
  | LAngle | LParen | LBrack | LBrace -> BracketL
  | RAngle | RParen | RBrack | RBrace -> BracketR
  | Turnstile -> Infix { assoc = Non; level = 1 }
  | Tilesturn -> Infix { assoc = Non; level = 2 }
  | SqArrow | SqArrowStar -> Infix { assoc = Right; level = 3 }
  | Colon | Tilde2 -> Infix { assoc = Left; level = 4 }
  | DoubleArrowSub | DoubleArrowLong -> Infix { assoc = Right; level = 5 }
  | Arrow | ArrowSub -> Infix { assoc = Right; level = 6 }
  | Semicolon -> Infix { assoc = Left; level = 7 }
  | Dot | Dot2 | Dot3 -> Infix { assoc = Left; level = 8 }
  | Backslash -> Infix { assoc = Left; level = 9 }
  | _ -> Plain

let closer_of : t -> t option = function
  | LAngle -> Some RAngle
  | LParen -> Some RParen
  | LBrack -> Some RBrack
  | LBrace -> Some RBrace
  | _ -> None

(* Lossy pretty-printing, omitting backticks on escaped atoms. *)
let string_of_atom = function
  | Atom id -> id
  | SilentAtom id -> "`" ^ id
  | Sub -> "<:"
  | Sup -> ":>"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | Tick -> "`"
  | DoubleQuote -> "\""
  | Underscore -> "_"
  | Arrow -> "->"
  | TickArrow -> "->"
  | ArrowSub -> "->_"
  | DoubleArrow -> "=>"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot -> "."
  | TickDot -> "."
  | Dot2 -> ".."
  | TickDot2 -> ".."
  | Dot3 -> "..."
  | TickDot3 -> "..."
  | Comma -> ","
  | Semicolon -> ";"
  | TickSemicolon -> ";"
  | Colon -> ":"
  | TickColon -> ":"
  | Hash -> "#"
  | Dollar -> "$"
  | At -> "@"
  | Quest -> "?"
  | Bang -> "!"
  | BangEq -> "!="
  | Tilde -> "~"
  | Tilde2 -> "~~"
  | LAngle -> "<"
  | TickLAngle -> "<"
  | LAngle2 -> "<<"
  | LAngleEq -> "<="
  | LAngle2Eq -> "<<="
  | RAngle -> ">"
  | TickRAngle -> ">"
  | RAngle2 -> ">>"
  | RAngleEq -> ">="
  | RAngle2Eq -> ">>="
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | TickLBrack -> "["
  | RBrack -> "]"
  | TickRBrack -> "]"
  | LBrace -> "{"
  | TickLBrace -> "{"
  | LBraceHashRBrace -> "{#}"
  | RBrace -> "}"
  | TickRBrace -> "}"
  | Plus -> "+"
  | Plus2 -> "++"
  | PlusEq -> "+="
  | Minus -> "-"
  | MinusEq -> "-="
  | Star -> "*"
  | StarEq -> "*="
  | Slash -> "/"
  | SlashEq -> "/="
  | Backslash -> "\\"
  | Percent -> "%"
  | PercentEq -> "%="
  | Eq -> "="
  | Eq2 -> "=="
  | Amp -> "&"
  | Amp2 -> "&&"
  | Amp3 -> "&&&"
  | AmpEq -> "&="
  | Up -> "^"
  | UpEq -> "^="
  | Bar -> "|"
  | Bar2 -> "||"
  | BarEq -> "|="
  | SPlus -> "|+|"
  | SPlusEq -> "|+|="
  | SMinus -> "|-|"
  | SMinusEq -> "|-|="

(* Debug printer: escaped variants keep their backticks. For round-trip checks. *)
let string_of_atom_exact : t -> string = function
  | TickArrow -> "`->"
  | TickDot -> "`."
  | TickDot2 -> "`.."
  | TickDot3 -> "`..."
  | TickSemicolon -> "`;"
  | TickColon -> "`:"
  | TickLAngle -> "``<"
  | TickRAngle -> "``>"
  | TickLBrack -> "``["
  | TickRBrack -> "``]"
  | TickLBrace -> "``{"
  | TickRBrace -> "``}"
  | a -> string_of_atom a
