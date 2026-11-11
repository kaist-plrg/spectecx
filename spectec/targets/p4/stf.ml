module Location = Common.Location

type expect = { port : int; hex : string; exact : bool }

type key_value =
  | Hex of string
  | Binary of string
  | Decimal of string
  | Prefix of { hex : string; length : int }

type action = { name : string; args : (string * Bigint.t) list }

type statement =
  | Packet of { port : int; hex : string }
  | Expect of expect
  | Add of {
      table : string;
      priority : Bigint.t option;
      keys : (string * key_value) list;
      action : action;
    }
  | SetDefault of { table : string; action : action }

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_decimal text =
  text <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) text

let parse_hex ~wildcard hex =
  let hex = String.uppercase_ascii hex in
  if String.for_all (fun c -> is_hex_digit c || (wildcard && c = '*')) hex then
    Some hex
  else None

let digits_after_prefix ~letter text =
  if
    String.length text > 2
    && text.[0] = '0'
    && Char.lowercase_ascii text.[1] = letter
  then Some (String.sub text 2 (String.length text - 2))
  else None

let parse_number text =
  match
    (digits_after_prefix ~letter:'x' text, digits_after_prefix ~letter:'b' text)
  with
  | Some digits, _ ->
      Option.map (fun hex -> Hex hex) (parse_hex ~wildcard:true digits)
  | None, Some digits
    when String.for_all (fun c -> c = '0' || c = '1' || c = '*') digits ->
      Some (Binary digits)
  | None, None when is_decimal text -> Some (Decimal text)
  | _ -> None

let parse_integer text =
  match parse_number text with
  | Some (Hex digits) when not (String.contains digits '*') ->
      Some (Bigint.of_string ("0x" ^ digits))
  | Some (Binary digits) when not (String.contains digits '*') ->
      Some (Bigint.of_string ("0b" ^ digits))
  | Some (Decimal digits) -> Some (Bigint.of_string digits)
  | _ -> None

let parse_key_value text =
  match String.split_on_char '/' text with
  | [ value; length ] -> (
      match
        (digits_after_prefix ~letter:'x' value, int_of_string_opt length)
      with
      | Some digits, Some length when length >= 0 ->
          Option.map
            (fun hex -> Prefix { hex; length })
            (parse_hex ~wildcard:false digits)
      | _ -> None)
  | [ number ] -> parse_number number
  | _ -> None

let parse_key word =
  match String.split_on_char ':' word with
  | [ name; value ] when name <> "" ->
      Option.map (fun value -> (name, value)) (parse_key_value value)
  | _ -> None

(* Keys end at the first word that opens the action's argument list. *)
let rec parse_keys keys = function
  | word :: words when not (String.contains word '(') ->
      Option.bind (parse_key word) (fun key -> parse_keys (key :: keys) words)
  | words -> Some (List.rev keys, words)

let parse_action_arg arg =
  match String.split_on_char ':' arg with
  | [ name; number ] when name <> "" ->
      Option.map (fun number -> (name, number)) (parse_integer number)
  | _ -> None

(* An action is written [name(arg:number, ...)], possibly across words. *)
let parse_action words =
  let text = String.concat "" words in
  match String.index_opt text '(' with
  | Some lparen when lparen > 0 && String.ends_with ~suffix:")" text ->
      let name = String.sub text 0 lparen in
      let args =
        String.sub text (lparen + 1) (String.length text - lparen - 2)
      in
      let args =
        if args = "" then Some []
        else
          List.fold_right
            (fun arg args ->
              Option.bind args (fun args ->
                  Option.map (fun arg -> arg :: args) (parse_action_arg arg)))
            (String.split_on_char ',' args)
            (Some [])
      in
      Option.map (fun args -> { name; args }) args
  | _ -> None

let parse_add words ~table =
  let priority, words =
    match words with
    | word :: words when is_decimal word -> (Some (Bigint.of_string word), words)
    | _ -> (None, words)
  in
  match parse_keys [] words with
  | Some ((_ :: _ as keys), words) -> (
      match parse_action words with
      | Some action -> Ok (Add { table; priority; keys; action })
      | None -> Error "malformed add statement")
  | _ -> Error "malformed add statement"

let parse_statement words =
  match words with
  | "packet" :: port :: words -> (
      let hex = String.concat "" words in
      match (int_of_string_opt port, parse_hex ~wildcard:false hex) with
      | Some port, Some hex when port >= 0 -> Ok (Packet { port; hex })
      | _ -> Error "malformed packet statement")
  | "expect" :: port :: words -> (
      let hex = String.concat "" words in
      let exact = String.ends_with ~suffix:"$" hex in
      let hex =
        if exact then String.sub hex 0 (String.length hex - 1) else hex
      in
      match (int_of_string_opt port, parse_hex ~wildcard:true hex) with
      | Some port, Some hex when port >= 0 -> Ok (Expect { port; hex; exact })
      | _ -> Error "malformed expect statement")
  | "add" :: table :: words -> parse_add words ~table
  | "setdefault" :: table :: words -> (
      match parse_action words with
      | Some action -> Ok (SetDefault { table; action })
      | None -> Error "malformed setdefault statement")
  | command :: _ -> Error ("unsupported STF statement: " ^ command)
  | [] -> Error "empty STF statement"

let words_of_line line =
  let line =
    match String.index_opt line '#' with
    | Some idx -> String.sub line 0 idx
    | None -> line
  in
  String.split_on_char ' ' line
  |> List.concat_map (String.split_on_char '\t')
  |> List.filter (fun word -> word <> "")

let parse_lines (lines : string list) : (statement list, string) result =
  let rec parse statements = function
    | [] -> Ok (List.rev statements)
    | line :: lines -> (
        match words_of_line line with
        | [] -> parse statements lines
        | words -> (
            match parse_statement words with
            | Ok statement -> parse (statement :: statements) lines
            | Error message -> Error message))
  in
  parse [] lines

let parse_file (filename : string) : statement list Spectec.Task.result =
  let error message =
    Error (Spectec.Error.TaskParseError (Location.File filename, message))
  in
  match In_channel.with_open_text filename In_channel.input_lines with
  | lines -> Result.fold ~ok:Result.ok ~error (parse_lines lines)
  | exception Sys_error message -> error message
