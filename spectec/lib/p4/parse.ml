open Error

let preprocess (includes : string list) (filename : string) =
  try Preprocessor.preprocess includes filename
  with _ -> "preprocessor error" |> error_no_region

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Lexing.from_string file
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error_no_region

let parse (lexbuf : Lexing.lexbuf) =
  let debug_level = Debug_config.get_parser_debug_level () in
  try
    match debug_level with
    | Debug_config.Quiet -> Parser.p4program Lexer.lexer lexbuf
    | _ -> Parser_debug.debug_parse Lexer.lexer lexbuf
  with
  | Parser.Error ->
      let info = Lexer.info lexbuf in
      let msg = Format.asprintf "syntax error" in
      error (Source.to_region info) msg
  | e -> raise e

let parse_string (filename : string) (str : string) : Il.value =
  (* assume str is preprocessed *)
  let tokens = lex filename str in
  parse tokens

let parse_file (includes : string list) (filename : string) : Il.value =
  let program = preprocess includes filename in
  parse_string filename program
