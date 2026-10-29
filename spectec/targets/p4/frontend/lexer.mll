(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
*)

{
open Lexing
open Common.Source
open Context
open Lang
open Il
open Il.Value
open Il.Case
open Parser
module F = Format

exception Error of string

let debug_channel = ref stderr
let set_debug_channel ch = debug_channel := ch
let lexer_debug_enabled () = Debug_config.lexer_debug_enabled Debug_config.Basic

let debug_print fmt =
  if Debug_config.lexer_debug_enabled Debug_config.Basic then
    Printf.fprintf !debug_channel fmt
  else
    Printf.ifprintf !debug_channel fmt

let debug_token lexeme =
  debug_print "%s" lexeme

type lexer_state =
  (* Nothing to recall from the previous tokens *)
  | SRegular
  | SRangle of Lexing.position
  | SPragma
  (* We have seen a template *)
  | STemplate
  (* We have seen an identifier:
   * we have just emitted a [NAME] token.
   * The next token will be either [IDENTIFIER] or [TYPENAME],
   * depending on what kind of identifier this is *)
  | SIdent of string * lexer_state
    let lexer_state = ref SRegular

let reset () =
  Context.reset ();
  lexer_state := SRegular

let set_line lexbuf line =
  let position = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { position with pos_lnum = line }

let set_start_of_line lexbuf bol =
  let position = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { position with pos_bol = bol }

let set_lexer_debug_channel ch = set_debug_channel ch

let at lexbuf =
  region_of_positions (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

let follows_position position_left position_right : bool =
  position_left.pos_fname = position_right.pos_fname
  && position_left.pos_lnum = position_right.pos_lnum
  && position_left.pos_cnum = position_right.pos_cnum

let sanitize s =
  String.concat "" (String.split_on_char '_' s)

let strip_prefix s =
  let length = String.length s in
  assert (length > 2);
  String.sub s 2 (length - 2)

let parse_int at n =
  let i = Bigint.of_string (sanitize n) in
  NumV (`Int i) |> make_val (NumT `IntT) <<<| at

let parse_width_int at s n =
  (* [s] contains the decimal width followed by "w" or "s". *)
  let l_s = String.length s in
  let width = String.sub s 0 (l_s - 1) in
  let sign = String.sub s (l_s - 1) 1 in
  let pos column = { at.left with column } in
  let column = at.left.column in
  let width_at = { left = at.left; right = pos (column + l_s - 1) } in
  let sign_at = { left = pos (column + l_s - 1); right = pos (column + l_s) } in
  let value_at = { left = pos (column + l_s); right = at.right } in
  let i = Bigint.of_string (sanitize n) in
  let w = Bigint.of_string width in
  match sign with
    | "s" ->
      if (int_of_string width < 2)
      then raise (Error "signed integers must have width at least 2")
      else
        let value_width = NumV (`Nat w) |> make_val (NumT `NatT) <<<| width_at in
        let value_int = NumV (`Int i) |> make_val (NumT `IntT) <<<| value_at in
        [ arg value_width; kw ~at:sign_at "S"; arg value_int ]
        |> case_v ~var:"number" <<<| at
    | "w" ->
      let value_width = NumV (`Nat w) |> make_val (NumT `NatT) <<<| width_at in
      let value_int = NumV (`Int i) |> make_val (NumT `IntT) <<<| value_at in
      [ arg value_width; kw ~at:sign_at "W"; arg value_int ]
      |> case_v ~var:"number" <<<| at
    | _ ->
      raise (Error "Illegal integer constant")
}

let name = [ 'A'-'Z' 'a'-'z' '_' ] [ 'A'-'Z' 'a'-'z' '0'-'9' '_' ]*
let hex_number = '0' [ 'x' 'X' ] [ '0'-'9' 'a'-'f' 'A'-'F' '_' ]+
let dec_number = '0' [ 'd' 'D' ] [ '0'-'9' '_' ]+
let oct_number = '0' [ 'o' 'O' ] [ '0'-'7' '_' ]+
let bin_number = '0' [ 'b' 'B' ] [ '0' '1' '_' ]+
let int = [ '0'-'9' ] [ '0'-'9' '_' ]*
let sign = [ '0'-'9' ]+ [ 'w' 's' ]

let whitespace = [ ' ' '\t' '\012' '\r' ]

rule tokenize = parse
  | "/*"
      { debug_token "/*";
        match multiline_comment None lexbuf with
       | None -> tokenize lexbuf
       | Some _info -> PRAGMA_END }
  | "//"
      { singleline_comment lexbuf; PRAGMA_END }
  | '\n'
      { debug_token "⏎\n"; Lexing.new_line lexbuf; PRAGMA_END }
  | '"'
      { let start_p = lexeme_start_p lexbuf in
        let start_region = at lexbuf in
        let str, end_region = (string lexbuf) in
        debug_token ("\"" ^ str ^ "\"");
        let inner_region =
          { left = start_region.right; right = end_region.left }
        in
        let value = text str <<<| inner_region in
        lexbuf.lex_start_p <- start_p;
        STRING_LITERAL value
      }
  | whitespace
      { debug_token " "; tokenize lexbuf }
  | '#'
      { debug_token ""; preprocessor lexbuf ; tokenize lexbuf }
  | "@pragma"
      { debug_token "@pragma"; PRAGMA }
  | hex_number as n
      { debug_token n; NUMBER_INT (parse_int (at lexbuf) n, n) }
  | dec_number as n
      { debug_token n; NUMBER_INT (parse_int (at lexbuf) (strip_prefix n), n) }
  | oct_number as n
      { debug_token n; NUMBER_INT (parse_int (at lexbuf) n, n) }
  | bin_number as n
      { debug_token n; NUMBER_INT (parse_int (at lexbuf) n, n) }
  | int as n
      { debug_token n; NUMBER_INT (parse_int (at lexbuf) n, n) }
  | (sign as s) (hex_number as n)
      { NUMBER (parse_width_int (at lexbuf) s n, n) }
  | (sign as s) (dec_number as n)
      { NUMBER (parse_width_int (at lexbuf) s (strip_prefix n), n) }
  | (sign as s) (oct_number as n)
      { NUMBER (parse_width_int (at lexbuf) s n, n) }
  | (sign as s) (bin_number as n)
      { NUMBER (parse_width_int (at lexbuf) s n, n) }
  | (sign as s) (int as n)
      { NUMBER (parse_width_int (at lexbuf) s n, n) }
  | "abstract"
      { debug_token "abstract"; ABSTRACT }
  | "action"
      { debug_token "action"; ACTION }
  | "actions"
      { debug_token "actions"; ACTIONS }
  | "apply"
      { debug_token "apply"; APPLY }
  | "bool"
      { debug_token "bool"; BOOL }
  | "bit"
      { debug_token "bit"; BIT }
  | "break"
      { debug_token "break"; BREAK }
  | "const"
      { debug_token "const"; CONST }
  | "continue"
      { debug_token "continue"; CONTINUE }
  | "control"
      { debug_token "control"; CONTROL }
  | "default"
      { debug_token "default"; DEFAULT }
  | "else"
      { debug_token "else"; ELSE }
  | "entries"
      { debug_token "entries"; ENTRIES }
  | "enum"
      { debug_token "enum"; ENUM }
  | "error"
      { debug_token "error"; ERROR }
  | "exit"
      { debug_token "exit"; EXIT }
  | "extern"
      { debug_token "extern"; EXTERN }
  | "header"
      { debug_token "header"; HEADER }
  | "header_union"
      { debug_token "header_union"; HEADER_UNION }
  | "true"
      { debug_token "true"; TRUE }
  | "false"
      { debug_token "false"; FALSE }
  | "for"
      { debug_token "for"; FOR }
  | "if"
      { debug_token "if"; IF }
  | "in"
      { debug_token "in"; IN }
  | "inout"
      { debug_token "inout"; INOUT }
  | "int"
      { debug_token "int"; INT }
  | "key"
      { debug_token "key"; KEY }
  | "list"
      { debug_token "list"; LIST }
  | "match_kind"
      { debug_token "match_kind"; MATCH_KIND }
  | "out"
      { debug_token "out"; OUT }
  | "parser"
      { debug_token "parser"; PARSER }
  | "package"
      { debug_token "package"; PACKAGE }
  | "pragma"
      { debug_token "pragma"; PRAGMA }
  | "priority"
      { debug_token "priority"; PRIORITY }
  | "return"
      { debug_token "return"; RETURN }
  | "select"
      { debug_token "select"; SELECT }
  | "state"
      { debug_token "state"; STATE }
  | "string"
      { debug_token "string"; STRING }
  | "struct"
      { debug_token "struct"; STRUCT }
  | "switch"
      { debug_token "switch"; SWITCH }
  | "table"
      { debug_token "table"; TABLE }
  | "this"
      { debug_token "this"; THIS }
  | "transition"
      { debug_token "transition"; TRANSITION }
  | "tuple"
      { debug_token "tuple"; TUPLE }
  | "typedef"
      { debug_token "typedef"; TYPEDEF }
  | "type"
      { debug_token "type"; TYPE }
  | "value_set"
      { debug_token "value_set"; VALUE_SET }
  | "varbit"
      { debug_token "varbit"; VARBIT }
  | "void"
      { debug_token "void"; VOID }
  | "_"
      { debug_token "_"; DONTCARE }
  | name
      { let s = Lexing.lexeme lexbuf in
        debug_token s;
        let value = text s <<<| at lexbuf in
        NAME value }
  | "<="
      { debug_token "<="; LE }
  | ">="
      { debug_token ">="; GE }
  | "<<"
      { debug_token "<<"; SHL }
  | "&&"
      { debug_token "&&"; AND }
  | "||"
      { debug_token "||"; OR }
  | "!="
      { debug_token "!="; NE }
  | "=="
      { debug_token "=="; EQ }
  | "+"
      { debug_token "+"; PLUS }
  | "-"
      { debug_token "-"; MINUS }
  | "|+|"
      { debug_token "|+|"; PLUS_SAT }
  | "|-|"
      { debug_token "|-|"; MINUS_SAT }
  | "*"
      { debug_token "*"; MUL }
  | "{#}"
      { debug_token "{#}"; INVALID }
  | "/"
      { debug_token "/"; DIV }
  | "%"
      { debug_token "%"; MOD }
  | "|"
      { debug_token "|"; BIT_OR }
  | "&"
      { debug_token "&"; BIT_AND }
  | "^"
      { debug_token "^"; BIT_XOR }
  | "~"
      { debug_token "~"; COMPLEMENT }
  | "["
      { debug_token "["; L_BRACKET }
  | "]"
      { debug_token "]"; R_BRACKET }
  | "{"
      { debug_token "{"; L_BRACE }
  | "}"
      { debug_token "}"; R_BRACE }
  | "<"
      { debug_token "<"; L_ANGLE }
  | ">"
      { debug_token ">"; R_ANGLE }
  | "("
      { debug_token "("; L_PAREN }
  | ")"
      { debug_token ")"; R_PAREN }
  | "!"
      { debug_token "!"; NOT }
  | ":"
      { debug_token ":"; COLON }
  | ","
      { debug_token ","; COMMA }
  | "?"
      { debug_token "?"; QUESTION }
  | "."
      { debug_token "."; DOT }
  | "="
      { debug_token "="; ASSIGN }
  | ";"
      { debug_token ";"; SEMICOLON }
  | "@"
      { debug_token "@"; AT }
  | "++"
      { debug_token "++"; PLUSPLUS }
  | "&&&"
      { debug_token "&&&"; MASK }
  | "..."
      { debug_token "..."; DOTS }
  | ".."
      { debug_token ".."; RANGE }
  | "+="
      { debug_token "+="; PLUS_ASSIGN }
  | "|+|="
      { debug_token "|+|="; PLUS_SAT_ASSIGN }
  | "-="
      { debug_token "-="; MINUS_ASSIGN }
  | "|-|="
      { debug_token "|-|="; MINUS_SAT_ASSIGN }
  | "*="
      { debug_token "*="; MUL_ASSIGN }
  | "/="
      { debug_token "/="; DIV_ASSIGN }
  | "%="
      { debug_token "%="; MOD_ASSIGN }
  | "<<="
      { debug_token "<<="; SHL_ASSIGN }
  | ">>="
      { debug_token ">>="; SHR_ASSIGN }
  | "&="
      { debug_token "&="; BIT_AND_ASSIGN }
  | "^="
      { debug_token "^="; BIT_XOR_ASSIGN }
  | "|="
      { debug_token "|="; BIT_OR_ASSIGN }
  | eof
      { debug_token "EOF"; END }
  | _
      { let s = lexeme lexbuf in
        debug_token s;
        let value = text s <<<| at lexbuf in
        UNEXPECTED_TOKEN value }

and string = parse
  | eof
      { raise (Error "File ended while reading a string literal") }
  | "\\\""
      { let rest, end_region = (string lexbuf) in
        ("\"" ^ rest, end_region) }
  | '\\' 'n'
      { let rest, end_region = (string lexbuf) in
        ("\n" ^ rest, end_region) }
  | '\\' '\\'
      { let rest, end_region = (string lexbuf) in
        ("\\" ^ rest, end_region) }
  | '\n'
      { Lexing.new_line lexbuf;
        let rest, end_region = (string lexbuf) in
        ("\n" ^ rest, end_region) }
  | '\\' _ as c
      { raise (Error ("Escape sequences not yet supported: \\" ^ c)) }
  | '"'
      { ("", at lexbuf) }
  | _ as chr
      { let rest, end_region = (string lexbuf) in
        ((String.make 1 chr) ^ rest, end_region) }

(* Preprocessor annotations indicate line and filename *)
and preprocessor = parse
  | ' '
      { preprocessor lexbuf }
  | int
      { let line = int_of_string (lexeme lexbuf) in
        set_line lexbuf line; preprocessor lexbuf }
  | '"'
      { preprocessor_string lexbuf }
  | '\n'
      { set_start_of_line lexbuf (lexeme_end lexbuf) }
  | _
      { preprocessor lexbuf }

and preprocessor_string = parse
  | [^ '"'] * '"'
    { let filename = lexeme lexbuf in
      let filename = String.sub filename 0 (String.length filename - 1) in
      Lexing.set_filename lexbuf filename;
      preprocessor_column lexbuf }

(* Once a filename has been recognized, ignore the rest of the line *)
and preprocessor_column = parse
  | ' '
      { preprocessor_column lexbuf }
  | '\n'
      { set_start_of_line lexbuf (lexeme_end lexbuf) }
  | eof
      { () }
  | _
      { preprocessor_column lexbuf }

(* Multi-line comment terminated by "*/" *)
and multiline_comment opt = parse
  | "*/"   { opt }
  | eof    { failwith "unterminated comment" }
  | '\n'   { Lexing.new_line lexbuf; multiline_comment (Some(at lexbuf)) lexbuf }
  | _      { multiline_comment opt lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { Lexing.new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

{
let rec lexer (lexbuf:lexbuf): token =
   match !lexer_state with
    | SIdent(id, next) ->
      begin match get_kind id with
      | TypeName true ->
        lexer_state := STemplate;
        TYPENAME
      | Ident true ->
        lexer_state := STemplate;
        IDENTIFIER
      | TypeName false ->
        lexer_state := next;
        TYPENAME
      | Ident false ->
        lexer_state := next;
        IDENTIFIER
      end
    | SRangle endp1 ->
      begin match tokenize lexbuf with
      | R_ANGLE when follows_position endp1 lexbuf.lex_start_p ->
        lexer_state := SRegular;
        R_ANGLE_SHIFT
      | PRAGMA as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END ->
        lexer_state := SRegular;
        lexer lexbuf
      | NAME value as token ->
        let text = get_text value in
        lexer_state := SIdent (text, SRegular);
        token
      | token ->
        lexer_state := SRegular;
        token
      end
    | SRegular ->
      begin match tokenize lexbuf with
      | NAME value as token ->
        let text = get_text value in
        lexer_state := SIdent (text, SRegular);
        token
      | PRAGMA as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END ->
        lexer lexbuf
      | R_ANGLE as token ->
        lexer_state := SRangle lexbuf.lex_curr_p;
        token
      | token ->
        lexer_state := SRegular;
        token
       end
    | STemplate ->
      begin match tokenize lexbuf with
      | L_ANGLE -> L_ANGLE_ARGS
      | NAME value as token ->
        let text = get_text value in
        lexer_state := SIdent (text, SRegular);
        token
      | PRAGMA as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END -> lexer lexbuf
      | R_ANGLE as token ->
        lexer_state := SRangle lexbuf.lex_curr_p;
        token
      | token ->
        lexer_state := SRegular;
        token
       end
    | SPragma ->
      begin match tokenize lexbuf with
      | PRAGMA_END as token ->
         lexer_state := SRegular;
         token
      | NAME value as token ->
         let text = get_text value in
         lexer_state := SIdent(text, SPragma);
         token
      | token -> token
      end
}
