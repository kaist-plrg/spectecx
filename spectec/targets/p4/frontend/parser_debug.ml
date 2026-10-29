(*
 * Parser debugging utilities using Menhir's inspection API:
 *
 *   Debugs parser stack state and token consumption
 *)

module I = Parser.Incremental
module Engine = Parser.MenhirInterpreter
module P = Printf

let get_debug_level () = Debug_config.get_parser_debug_level ()

let token_name token =
  try
    match token with
    | Parser.ABSTRACT -> "abstract"
    | Parser.ACTION -> "action"
    | Parser.ACTIONS -> "actions"
    | Parser.APPLY -> "apply"
    | Parser.BOOL -> "bool"
    | Parser.BIT -> "bit"
    | Parser.BREAK -> "break"
    | Parser.CONST -> "const"
    | Parser.CONTINUE -> "continue"
    | Parser.CONTROL -> "control"
    | Parser.DEFAULT -> "default"
    | Parser.ELSE -> "else"
    | Parser.ENTRIES -> "entries"
    | Parser.ENUM -> "enum"
    | Parser.ERROR -> "error"
    | Parser.EXIT -> "exit"
    | Parser.EXTERN -> "extern"
    | Parser.HEADER -> "header"
    | Parser.HEADER_UNION -> "header_union"
    | Parser.IF -> "if"
    | Parser.IN -> "in"
    | Parser.INOUT -> "inout"
    | Parser.INT -> "int"
    | Parser.KEY -> "key"
    | Parser.LIST -> "list"
    | Parser.SELECT -> "select"
    | Parser.MATCH_KIND -> "match_kind"
    | Parser.OUT -> "out"
    | Parser.PACKAGE -> "package"
    | Parser.PARSER -> "parser"
    | Parser.PRIORITY -> "priority"
    | Parser.RETURN -> "return"
    | Parser.STATE -> "state"
    | Parser.STRING -> "string"
    | Parser.STRUCT -> "struct"
    | Parser.SWITCH -> "switch"
    | Parser.TABLE -> "table"
    | Parser.THIS -> "this"
    | Parser.TRANSITION -> "transition"
    | Parser.TUPLE -> "tuple"
    | Parser.TYPEDEF -> "typedef"
    | Parser.TYPE -> "type"
    | Parser.VALUE_SET -> "value_set"
    | Parser.VARBIT -> "varbit"
    | Parser.VOID -> "void"
    | Parser.TRUE -> "true"
    | Parser.FALSE -> "false"
    | Parser.FOR -> "for"
    | Parser.END -> "end"
    | Parser.TYPENAME -> "typename"
    | Parser.IDENTIFIER -> "identifier"
    | Parser.NAME s -> P.sprintf "name %s" (Lang.Il.Print.string_of_value s)
    | Parser.STRING_LITERAL _ -> "string_literal"
    | Parser.NUMBER _ -> "number"
    | Parser.LE -> "<="
    | Parser.GE -> ">="
    | Parser.SHL -> ">>"
    | Parser.AND -> "&"
    | Parser.OR -> "|"
    | Parser.NE -> "!="
    | Parser.EQ -> "=="
    | Parser.PLUS -> "+"
    | Parser.MINUS -> "-"
    | Parser.PLUS_SAT -> "PLUS_SAT"
    | Parser.MINUS_SAT -> "MINUS_SAT"
    | Parser.MUL -> "*"
    | Parser.INVALID -> "INVALID"
    | Parser.DIV -> "DIV"
    | Parser.MOD -> "MOD"
    | Parser.BIT_OR -> "BIT_OR"
    | Parser.BIT_AND -> "BIT_AND"
    | Parser.BIT_XOR -> "BIT_XOR"
    | Parser.COMPLEMENT -> "COMPLEMENT"
    | Parser.L_BRACKET -> "L_BRACKET"
    | Parser.R_BRACKET -> "R_BRACKET"
    | Parser.L_BRACE -> "L_BRACE"
    | Parser.R_BRACE -> "R_BRACE"
    | Parser.L_ANGLE -> "L_ANGLE"
    | Parser.L_ANGLE_ARGS -> "L_ANGLE_ARGS"
    | Parser.R_ANGLE -> "R_ANGLE"
    | Parser.R_ANGLE_SHIFT -> "R_ANGLE_SHIFT"
    | Parser.L_PAREN -> "L_PAREN"
    | Parser.R_PAREN -> "R_PAREN"
    | Parser.ASSIGN -> "ASSIGN"
    | Parser.COLON -> "COLON"
    | Parser.COMMA -> "COMMA"
    | Parser.QUESTION -> "QUESTION"
    | Parser.DOT -> "DOT"
    | Parser.NOT -> "NOT"
    | Parser.SEMICOLON -> "SEMICOLON"
    | Parser.AT -> "AT"
    | Parser.PLUSPLUS -> "PLUSPLUS"
    | Parser.DONTCARE -> "DONTCARE"
    | Parser.MASK -> "MASK"
    | Parser.DOTS -> "DOTS"
    | Parser.RANGE -> "RANGE"
    | Parser.PRAGMA -> "PRAGMA"
    | Parser.PRAGMA_END -> "PRAGMA_END"
    | Parser.UNEXPECTED_TOKEN _ -> "UNEXPECTED_TOKEN"
    | _ -> "unknown"
  with _ -> "UNKNOWN_TOKEN"

(* Recursively collect stack states using top and pop *)
let rec collect_stack env acc =
  match Parser.MenhirInterpreter.top env with
  | None -> List.rev acc
  | Some (Parser.MenhirInterpreter.Element (state, _, _, _)) -> (
      let state_num = Parser.MenhirInterpreter.number state in
      match Parser.MenhirInterpreter.pop env with
      | None -> List.rev (state_num :: acc)
      | Some env' -> collect_stack env' (state_num :: acc))

let print_state env =
  let current_state = Parser.MenhirInterpreter.current_state_number env in
  let states = collect_stack env [] in
  let debug_level = get_debug_level () in

  if Debug_config.debug_enabled debug_level Basic then
    Printf.printf "@State %d:\n" current_state;
  match states with
  | [] ->
      if Debug_config.debug_enabled debug_level Verbose then
        Printf.printf "+Stack empty\n"
  | _ ->
      if Debug_config.debug_enabled debug_level Verbose then
        Printf.printf "+Stack: [%s]\n"
          (String.concat "; " (List.map string_of_int states))

let debug_parse lexer lexbuf =
  let supplier = Engine.lexer_lexbuf_to_supplier lexer lexbuf in
  let checkpoint = I.p4program lexbuf.lex_curr_p in
  let debug_level = get_debug_level () in
  let rec loop checkpoint =
    (match checkpoint with
    | Engine.InputNeeded env -> print_state env
    | Engine.Shifting (env, _, _) -> print_state env
    | Engine.AboutToReduce (env, _) ->
        print_state env;
        if Debug_config.debug_enabled debug_level Verbose then
          Printf.printf "--- About to reduce\n"
    | Engine.HandlingError env ->
        print_state env;
        if Debug_config.debug_enabled debug_level Basic then
          Printf.printf "Parser: Handling error\n"
    | _ -> ());
    match checkpoint with
    | Engine.InputNeeded _env ->
        let token, _, _ = supplier () in
        if Debug_config.debug_enabled debug_level Verbose then
          Printf.printf "\n|-> Consuming token: %s\n\n" (token_name token);
        loop
          (Engine.offer checkpoint (token, Lexing.dummy_pos, Lexing.dummy_pos))
    | Engine.Shifting _ | Engine.AboutToReduce _ ->
        loop (Engine.resume checkpoint)
    | Engine.HandlingError _env ->
        if Debug_config.debug_enabled debug_level Basic then
          Printf.printf "Parser: Syntax error occurred\n";
        raise Parser.Error
    | Engine.Accepted v ->
        if Debug_config.debug_enabled debug_level Basic then
          Printf.printf "Parser: Parsing completed successfully\n";
        v
    | Engine.Rejected -> failwith "Parser: Rejected"
  in
  if Debug_config.debug_enabled debug_level Basic then
    Printf.printf "Parser: Starting parse with debug level %s\n"
      (match debug_level with
      | Quiet -> "quiet"
      | Basic -> "basic"
      | Verbose -> "verbose"
      | Full -> "full");
  loop checkpoint
