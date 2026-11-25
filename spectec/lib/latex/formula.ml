type font = Roman | Sans | Mono | Blackboard
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

type t =
  | Word of font * string
  | Numeral of string
  | Text of string
  | Symbol of symbol
  | Concat of t list
  | Seq of t list
  | Infix of { left : t; op : t; right : t }
  | Commas of t list
  | Delimited of delimiter * t
  | Script of { base : t; sub : t option; sup : t option }

type escaped = Text_and_math of string | Math_only of string

let escape (char : char) : escaped =
  match char with
  | '#' | '$' | '%' | '&' | '_' -> Text_and_math (Printf.sprintf "\\%c" char)
  | ' ' -> Text_and_math "\\ "
  | '{' -> Math_only "\\{"
  | '}' -> Math_only "\\}"
  | '\\' -> Math_only "\\backslash{}"
  | '^' -> Math_only "\\hat{}"
  (* Wrap [\sim] in [\mathord] so the tilde in the text [a~b] stays next to the
     letters. TeX normally spaces [\sim] as a relation. *)
  | '~' -> Math_only "\\mathord{\\sim}"
  | char -> Text_and_math (String.make 1 char)

let escape_math (text : string) : string =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun char ->
      match escape char with
      | Text_and_math command | Math_only command ->
          Buffer.add_string buffer command)
    text;
  Buffer.contents buffer

(* Use math commands outside [\texttt] for backslashes, carets, tildes
   and braces so LaTeX and MathJax can read the same escapes. *)
let string_of_text (text : string) : string =
  let buffer = Buffer.create (String.length text) in
  let run = Buffer.create (String.length text) in
  let flush_run () =
    if Buffer.length run > 0 then (
      Buffer.add_string buffer "\\texttt{";
      Buffer.add_buffer buffer run;
      Buffer.add_char buffer '}';
      Buffer.clear run)
  in
  String.iter
    (fun char ->
      match escape char with
      | Text_and_math command -> Buffer.add_string run command
      | Math_only command ->
          flush_run ();
          Buffer.add_string buffer command)
    text;
  flush_run ();
  Buffer.contents buffer

let command_of_font (font : font) : string =
  match font with
  | Roman -> "\\mathrm"
  | Sans -> "\\mathsf"
  | Mono -> "\\mathtt"
  | Blackboard -> "\\mathbb"

let string_of_symbol (symbol : symbol) : string =
  match symbol with
  | Equal -> "="
  | Not_equal -> "\\ne"
  | Less -> "<"
  | Greater -> ">"
  | Less_equal -> "\\le"
  | Greater_equal -> "\\ge"
  | Plus -> "+"
  | Minus -> "-"
  | Cdot -> "\\cdot"
  | Slash -> "/"
  | Mod -> "\\bmod"
  | Neg -> "\\neg"
  | Wedge -> "\\wedge"
  | Vee -> "\\vee"
  | Rightarrow -> "\\Rightarrow"
  | Leftrightarrow -> "\\Leftrightarrow"
  | Longrightarrow -> "\\Longrightarrow"
  | Hookrightarrow -> "\\hookrightarrow"
  | Hookrightarrow_star -> "\\mathrel{\\hookrightarrow^{\\ast}}"
  | To -> "\\to"
  | Vdash -> "\\vdash"
  | Dashv -> "\\dashv"
  | In -> "\\in"
  | Double_colon -> "\\mathbin{::}"
  | Double_plus -> "\\mathbin{+\\!\\!+}"
  | Less_colon -> "\\mathrel{<:}"
  | Colon_greater -> "\\mathrel{:>}"
  | Colon -> ":"
  | Colon_equal -> "\\mathrel{:=}"
  | Semicolon -> ";"
  | Dot -> "."
  | Two_dots -> ".."
  | Ellipsis -> "\\ldots"
  | Sim -> "\\sim"
  | Setminus -> "\\setminus"
  | Ast -> "\\ast"
  | Question -> "?"
  | Epsilon -> "\\epsilon"
  | Left_angle -> "\\langle"
  | Right_angle -> "\\rangle"
  | Left_paren -> "("
  | Right_paren -> ")"
  | Left_bracket -> "["
  | Right_bracket -> "]"
  | Left_brace -> "\\{"
  | Right_brace -> "\\}"
  | Operator text -> "\\mathbin{" ^ string_of_text text ^ "}"

(* Plain parentheses avoid the thin space TeX inserts after the name in
   [f\left(x\right)]. *)
let strings_of_delimiter (delimiter : delimiter) : string * string =
  let pair (opening : symbol) (closing : symbol) : string * string =
    (string_of_symbol opening, string_of_symbol closing)
  in
  match delimiter with
  | Paren -> pair Left_paren Right_paren
  | Bracket -> pair Left_bracket Right_bracket
  | Brace -> pair Left_brace Right_brace
  | Angle -> pair Left_angle Right_angle
  (* Mark the first bar as an opening delimiter so TeX spaces the minus
     in [|-x|] as a sign on [x]. *)
  | Bar -> ("\\mathopen{|}", "\\mathclose{|}")

let rec to_string (formula : t) : string =
  match formula with
  | Word (font, text) -> command_of_font font ^ "{" ^ escape_math text ^ "}"
  | Numeral digits -> digits
  | Text text -> string_of_text text
  | Symbol symbol -> string_of_symbol symbol
  | Concat parts -> String.concat "" (List.map to_string parts)
  | Seq items -> String.concat "\\," (List.map to_string items)
  | Infix { left; op; right } ->
      String.concat " " [ to_string left; to_string op; to_string right ]
  | Commas items -> String.concat ", " (List.map to_string items)
  | Delimited (delimiter, body) ->
      let opening, closing = strings_of_delimiter delimiter in
      opening ^ to_string body ^ closing
  | Script { base; sub; sup } ->
      let script marker = function
        | None -> ""
        | Some part -> marker ^ "{" ^ to_string part ^ "}"
      in
      (* Leave a relation such as [\to] unbraced so TeX keeps its wider
         spacing when it adds a subscript. *)
      let base =
        match base with
        (* Brace [..] so TeX attaches the script to the pair of dots. *)
        | Symbol Two_dots -> "{" ^ to_string base ^ "}"
        | Symbol _ -> to_string base
        | _ -> "{" ^ to_string base ^ "}"
      in
      base ^ script "_" sub ^ script "^" sup
