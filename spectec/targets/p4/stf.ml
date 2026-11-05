module Location = Common.Location

type expect = { port : int; hex : string; exact : bool }
type statement = Packet of { port : int; hex : string } | Expect of expect

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let parse_hex ~wildcard hex =
  let hex = String.uppercase_ascii hex in
  if String.for_all (fun c -> is_hex_digit c || (wildcard && c = '*')) hex then
    Some hex
  else None

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
