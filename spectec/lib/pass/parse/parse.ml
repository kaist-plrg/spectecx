open Error
module Source = Common.Source

let with_lexbuf name lexbuf start =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
  try start Lexer.token lexbuf
  with Parser.Error ->
    error (Lexer.region lexbuf) "syntax error: unexpected token"

let parse_file file : Lang.El.spec result =
  try
    let ic = open_in file in
    let spec =
      Fun.protect
        (fun () -> with_lexbuf file (Lexing.from_channel ic) Parser.spec)
        ~finally:(fun () -> close_in ic)
    in
    Ok spec
  with
  | ParseError e -> Error e
  | Sys_error msg -> Error (Source.region_of_file file, "i/o error: " ^ msg)

let parse_files filenames : Lang.El.spec result =
  let rec parse_files' acc = function
    | [] -> Ok (List.concat (List.rev acc))
    | file :: rest -> (
        match parse_file file with
        | Ok spec -> parse_files' (spec :: acc) rest
        | Error e -> Error e)
  in
  parse_files' [] filenames

type error = Error.error
type 'a result = 'a Error.result

let error_to_string = Error.to_string
let error_to_diagnostic = Error.to_diagnostic
