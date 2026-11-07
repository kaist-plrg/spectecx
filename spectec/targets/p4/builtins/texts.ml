module Xl = Lang.Xl
open Lang.Il
open Common.Annotated
open Builtins

(* dec $text_to_int(text) : int *)

let text_to_int ~at (text : string) : Value.t result =
  at |> ignore;
  let i = text |> int_of_string |> Bigint.of_int in
  Ok (Value.int i)

(* dec $int_to_text(int) : text *)

let int_to_text ~at (num : num) : Value.t result =
  at |> ignore;
  Ok (Value.text (Xl.Num.string_of_num num))

(* dec $split_text(text, text) : text* *)

let split_text ~at (text : string) (separator : string) : Value.t result =
  if String.length separator <> 1 then
    let msg =
      Printf.sprintf "split_text: separator %S is not one character" separator
    in
    Error (Error.runtime at msg)
  else
    let parts = String.split_on_char separator.[0] text in
    Ok (Value.list (Typ.text $ at) (List.map Value.text parts))

(* dec $strip_prefix(text, text) : text *)

let strip_prefix ~at (text : string) (prefix : string) : Value.t result =
  if String.starts_with ~prefix text then
    let text =
      String.sub text (String.length prefix)
        (String.length text - String.length prefix)
    in
    Ok (Value.text text)
  else
    let msg =
      Printf.sprintf "strip_prefix: text %S does not start with prefix %S" text
        prefix
    in
    Error (Error.runtime at msg)

(* dec $strip_suffix(text, text) : text *)

let strip_suffix ~at (text : string) (suffix : string) : Value.t result =
  if String.ends_with ~suffix text then
    let text = String.sub text 0 (String.length text - String.length suffix) in
    Ok (Value.text text)
  else
    let msg =
      Printf.sprintf "strip_suffix: text %S does not end with suffix %S" text
        suffix
    in
    Error (Error.runtime at msg)

(* dec $strip_all_whitespace(text) : text *)

let strip_all_whitespace ~at (text : string) : Value.t result =
  at |> ignore;
  let text = text |> String.split_on_char ' ' |> String.concat "" in
  Ok (Value.text text)

let builtins =
  [
    ("text_to_int", Define.T0.a1 Arg.text text_to_int);
    ("int_to_text", Define.T0.a1 Arg.num int_to_text);
    ("split_text", Define.T0.a2 Arg.text Arg.text split_text);
    ("strip_prefix", Define.T0.a2 Arg.text Arg.text strip_prefix);
    ("strip_suffix", Define.T0.a2 Arg.text Arg.text strip_suffix);
    ("strip_all_whitespace", Define.T0.a1 Arg.text strip_all_whitespace);
  ]
