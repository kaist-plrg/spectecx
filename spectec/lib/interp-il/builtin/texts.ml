open Il

(* dec $int_to_text(int) : text *)

let int_to_text ~at (num : num) : (Value.t, Err.t) result =
  at |> ignore;
  Ok (Value.text (Xl.Num.string_of_num num))

(* dec $strip_prefix(text, text) : text *)

let strip_prefix ~at (text : string) (prefix : string) : (Value.t, Err.t) result
    =
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
    Error (Err.runtime at msg)

(* dec $strip_suffix(text, text) : text *)

let strip_suffix ~at (text : string) (suffix : string) : (Value.t, Err.t) result
    =
  if String.ends_with ~suffix text then
    let text = String.sub text 0 (String.length text - String.length suffix) in
    Ok (Value.text text)
  else
    let msg =
      Printf.sprintf "strip_suffix: text %S does not end with suffix %S" text
        suffix
    in
    Error (Err.runtime at msg)

let builtins =
  [
    ("int_to_text", Define.T0.a1 Arg.num int_to_text);
    ("strip_prefix", Define.T0.a2 Arg.text Arg.text strip_prefix);
    ("strip_suffix", Define.T0.a2 Arg.text Arg.text strip_suffix);
  ]
