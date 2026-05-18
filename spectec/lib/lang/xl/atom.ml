[@@@ocamlformat "disable"]

(* Tick-payload conventions:
   - `Plain  : bare form     (e.g. `.`)
   - `Tick   : one-backtick  (e.g. `` `. ``)
   - `Tick2  : two-backtick  (e.g. `` ``< ``)
   Each constructor admits only the tick depths its source-level token set
   actually produces, so an unparseable depth is unrepresentable. *)

type t =
  | Atom of string                  (* atomid *)
  | SilentAtom of string            (* `atomid *)
  | Sub                             (* `<:` *)
  | Sup                             (* `:>` *)
  | Turnstile                       (* `|-` *)
  | Tilesturn                       (* `-|` *)
  | Tick                            (* ```` *)
  | DoubleQuote                     (* ``''` *)
  | Underscore                      (* ``_` *)
  | Arrow of [ `Plain | `Tick ]     (* `->` or ``->` *)
  | ArrowSub                        (* `->_` *)
  | DoubleArrow                     (* ``=>` *)
  | DoubleArrowSub                  (* ``=>_` *)
  | DoubleArrowLong                 (* ``==>` *)
  | SqArrow                         (* `~>` *)
  | SqArrowStar                     (* `~>*` *)
  | Dot of [ `Plain | `Tick ]       (* `.` or ``.` *)
  | Dot2 of [ `Plain | `Tick ]      (* `..` or ``..` *)
  | Dot3 of [ `Plain | `Tick ]      (* `...` or ``...` *)
  | Comma                           (* ``,` *)
  | Semicolon of [ `Plain | `Tick ] (* `;` or ``;` *)
  | Colon of [ `Plain | `Tick ]     (* `:` or ``:` *)
  | Hash                            (* ``#` *)
  | Dollar                          (* ``$` *)
  | At                              (* ``@` *)
  | Quest                           (* ``?` *)
  | Bang                            (* ``!` *)
  | BangEq                          (* ``!=` *)
  | Tilde                           (* ``~` *)
  | Tilde2                          (* `~~` *)
  | LAngle of [ `Tick | `Tick2 ]    (* ``<` or ```<` *)
  | LAngle2                         (* `<<` *)
  | LAngleEq                        (* ``<=` *)
  | LAngle2Eq                       (* `<<=` *)
  | RAngle of [ `Plain | `Tick2 ]   (* `>` or ```>` *)
  | RAngle2                         (* `>>` *)
  | RAngleEq                        (* ``>=` *)
  | RAngle2Eq                       (* `>>=` *)
  | LParen                          (* ``(` *)
  | RParen                          (* ``)` *)
  | LBrack of [ `Tick | `Tick2 ]    (* ``[` or ```[` *)
  | RBrack of [ `Plain | `Tick2 ]   (* `]` or ```]` *)
  | LBrace of [ `Tick | `Tick2 ]    (* ``{` or ```{` *)
  | LBraceHashRBrace                (* `{#}` *)
  | RBrace of [ `Plain | `Tick2 ]   (* `}` or ```}` *)
  | Plus                            (* ``+` *)
  | Plus2                           (* ``++` *)
  | PlusEq                          (* ``+=` *)
  | Minus                           (* ``-` *)
  | MinusEq                         (* ``-=` *)
  | Star                            (* ``*` *)
  | StarEq                          (* ``*=` *)
  | Slash                           (* ``/` *)
  | SlashEq                         (* ``/=` *)
  | Backslash                       (* ``\` *)
  | Percent                         (* ``%` *)
  | PercentEq                       (* ``%=` *)
  | Eq                              (* ``=` *)
  | Eq2                             (* `==` *)
  | Amp                             (* ``&` *)
  | Amp2                            (* ``&&` *)
  | Amp3                            (* ``&&&` *)
  | AmpEq                           (* ``&=` *)
  | Up                              (* ``^` *)
  | UpEq                            (* ``^=` *)
  | Bar                             (* ``|` *)
  | Bar2                            (* ``||` *)
  | BarEq                           (* ``|=` *)
  | SPlus                           (* ``|+|` *)
  | SPlusEq                         (* ``|+|=` *)
  | SMinus                          (* ``|-|` *)
  | SMinusEq                        (* ``|-|=` *)
[@@@ocamlformat "enable"]

let compare atom_a atom_b = compare atom_a atom_b
let eq atom_a atom_b = compare atom_a atom_b = 0

(* Precedence mirrors parser.mly: relop atoms (levels 1-4) are looser than
   infixop atoms (levels 5-9). Higher level = tighter. Ticked forms of infix
   atoms fall through to Plain kind, since escape syntax suppresses the
   operator role. *)

type assoc = Left | Right | Non

type kind =
  | Plain
  | Infix of { assoc : assoc; level : int }
  | BracketL
  | BracketR

let kind : t -> kind = function
  | LAngle `Tick | LParen | LBrack `Tick | LBrace `Tick -> BracketL
  | RAngle `Plain | RParen | RBrack `Plain | RBrace `Plain -> BracketR
  | Turnstile -> Infix { assoc = Non; level = 1 }
  | Tilesturn -> Infix { assoc = Non; level = 2 }
  | SqArrow | SqArrowStar -> Infix { assoc = Right; level = 3 }
  | Colon `Plain | Tilde2 -> Infix { assoc = Left; level = 4 }
  | DoubleArrowSub | DoubleArrowLong -> Infix { assoc = Right; level = 5 }
  | Arrow `Plain | ArrowSub -> Infix { assoc = Right; level = 6 }
  | Semicolon `Plain -> Infix { assoc = Left; level = 7 }
  | Dot `Plain | Dot2 `Plain | Dot3 `Plain -> Infix { assoc = Left; level = 8 }
  | Backslash -> Infix { assoc = Left; level = 9 }
  | _ -> Plain

let closer_of : t -> t option = function
  | LAngle `Tick -> Some (RAngle `Plain)
  | LParen -> Some RParen
  | LBrack `Tick -> Some (RBrack `Plain)
  | LBrace `Tick -> Some (RBrace `Plain)
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
  | Arrow _ -> "->"
  | ArrowSub -> "->_"
  | DoubleArrow -> "=>"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot _ -> "."
  | Dot2 _ -> ".."
  | Dot3 _ -> "..."
  | Comma -> ","
  | Semicolon _ -> ";"
  | Colon _ -> ":"
  | Hash -> "#"
  | Dollar -> "$"
  | At -> "@"
  | Quest -> "?"
  | Bang -> "!"
  | BangEq -> "!="
  | Tilde -> "~"
  | Tilde2 -> "~~"
  | LAngle _ -> "<"
  | LAngle2 -> "<<"
  | LAngleEq -> "<="
  | LAngle2Eq -> "<<="
  | RAngle _ -> ">"
  | RAngle2 -> ">>"
  | RAngleEq -> ">="
  | RAngle2Eq -> ">>="
  | LParen -> "("
  | RParen -> ")"
  | LBrack _ -> "["
  | RBrack _ -> "]"
  | LBrace _ -> "{"
  | LBraceHashRBrace -> "{#}"
  | RBrace _ -> "}"
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

(* Debug printer: ticked variants keep their backticks. For round-trip checks. *)
let string_of_atom_exact : t -> string = function
  | Arrow `Tick -> "`->"
  | Dot `Tick -> "`."
  | Dot2 `Tick -> "`.."
  | Dot3 `Tick -> "`..."
  | Semicolon `Tick -> "`;"
  | Colon `Tick -> "`:"
  | LAngle `Tick2 -> "``<"
  | RAngle `Tick2 -> "``>"
  | LBrack `Tick2 -> "``["
  | RBrack `Tick2 -> "``]"
  | LBrace `Tick2 -> "``{"
  | RBrace `Tick2 -> "``}"
  | a -> string_of_atom a

(* NOTE: ".", ":", ";", "..", "..." map to their `Tick variants rather than
   `Plain. This is because the helper is used by user-facing parsers. *)
let of_string : string -> t = function
  | "<:" -> Sub
  | ":>" -> Sup
  | "|-" -> Turnstile
  | "-|" -> Tilesturn
  | "`" -> Tick
  | "\"" -> DoubleQuote
  | "_" -> Underscore
  | "->" -> Arrow `Plain
  | "`->" -> Arrow `Tick
  | "->_" -> ArrowSub
  | "=>" -> DoubleArrow
  | "=>_" -> DoubleArrowSub
  | "~>" -> SqArrow
  | "~>*" -> SqArrowStar
  | "." | "`." -> Dot `Tick
  | ".." | "`.." -> Dot2 `Tick
  | "..." | "`..." -> Dot3 `Tick
  | "," -> Comma
  | ";" | "`;" -> Semicolon `Tick
  | ":" | "`:" -> Colon `Tick
  | "#" -> Hash
  | "$" -> Dollar
  | "@" -> At
  | "?" -> Quest
  | "!" -> Bang
  | "!=" -> BangEq
  | "~" -> Tilde
  | "~~" -> Tilde2
  | "<" -> LAngle `Tick
  | "``<" -> LAngle `Tick2
  | "<<" -> LAngle2
  | "<=" -> LAngleEq
  | "<<=" -> LAngle2Eq
  | ">" -> RAngle `Plain
  | "``>" -> RAngle `Tick2
  | ">>" -> RAngle2
  | ">=" -> RAngleEq
  | ">>=" -> RAngle2Eq
  | "(" -> LParen
  | ")" -> RParen
  | "[" -> LBrack `Tick
  | "``[" -> LBrack `Tick2
  | "]" -> RBrack `Plain
  | "``]" -> RBrack `Tick2
  | "{" -> LBrace `Tick
  | "``{" -> LBrace `Tick2
  | "{#}" -> LBraceHashRBrace
  | "}" -> RBrace `Plain
  | "``}" -> RBrace `Tick2
  | "+" -> Plus
  | "++" -> Plus2
  | "+=" -> PlusEq
  | "-" -> Minus
  | "-=" -> MinusEq
  | "*" -> Star
  | "*=" -> StarEq
  | "/" -> Slash
  | "/=" -> SlashEq
  | "\\" -> Backslash
  | "%" -> Percent
  | "%=" -> PercentEq
  | "=" -> Eq
  | "==" -> Eq2
  | "&" -> Amp
  | "&&" -> Amp2
  | "&&&" -> Amp3
  | "&=" -> AmpEq
  | "^" -> Up
  | "^=" -> UpEq
  | "|" -> Bar
  | "||" -> Bar2
  | "|=" -> BarEq
  | "|+|" -> SPlus
  | "|+|=" -> SPlusEq
  | "|-|" -> SMinus
  | "|-|=" -> SMinusEq
  | s when String.length s > 0 && s.[0] = '`' ->
      SilentAtom (String.sub s 1 (String.length s - 1))
  | s -> Atom s
