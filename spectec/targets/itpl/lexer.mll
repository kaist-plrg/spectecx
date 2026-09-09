{
open Lexing
open Parse
open Common.Source

exception Parse_error of region * string

let position p =
  { file = p.pos_fname; line = p.pos_lnum; column = p.pos_cnum - p.pos_bol }

let region lexbuf =
  { left = position (lexeme_start_p lexbuf); right = position (lexeme_end_p lexbuf) }
}

let digit = ['0'-'9']
let initial = ['a'-'z' 'A'-'Z' '_']
let subsequent = initial | digit | '\''

rule token = parse
| [' ' '\t' '\r']+ { token lexbuf }
| '\n' { new_line lexbuf; token lexbuf }
| digit+ as n { NUM (Bigint.of_string n) }
| "val" { VAL }
| "in" { IN }
| "def" { DEF }
| "fun" { FUN }
| "if0" { IF0 }
| "then" { THEN }
| "else" { ELSE }
| "box" { BOX }
| "vcc" { VCC }
| initial subsequent* as x { ID x }
| '+' { PLUS }
| '-' { MINUS }
| '(' { LPAR }
| ')' { RPAR }
| '=' { EQUAL }
| ';' { SEMI }
| "->" { ARROW }
| '!' { BANG }
| ":=" { ASSIGN }
| eof { EOF }
| _ as c { raise (Parse_error (region lexbuf, Printf.sprintf "unexpected character %C" c)) }

{
let parse parser ~filename lexbuf =
  set_filename lexbuf filename;
  try Ok (parser token lexbuf) with
  | Parsing.Parse_error ->
      Error (Spectec.Error.TaskParseError (region lexbuf, "syntax error"))
  | Parse_error (at, message) ->
      Error (Spectec.Error.TaskParseError (at, message))

let parse_file parser filename =
  let channel = open_in filename in
  Fun.protect ~finally:(fun () -> close_in channel)
    (fun () -> parse parser ~filename (from_channel channel))

let parse_string parser ~spec:_ ~filename contents =
  parse parser ~filename (from_string contents) |> Result.map (fun input -> [input])
}
