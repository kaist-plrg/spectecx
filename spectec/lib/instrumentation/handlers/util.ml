(* Common string utilities for instrumentation handlers. *)

(* Normalize whitespace to single spaces *)
let normalize_whitespace s =
  let buf = Buffer.create (String.length s) in
  let last_ws = ref false in
  String.iter
    (fun c ->
      if c = ' ' || c = '\n' || c = '\t' || c = '\r' then (
        if not !last_ws then Buffer.add_char buf ' ';
        last_ws := true)
      else (
        Buffer.add_char buf c;
        last_ws := false))
    s;
  Buffer.contents buf

(* Truncate string to max length with "..." *)
let truncate max_len s =
  if String.length s > max_len then String.sub s 0 (max_len - 3) ^ "..." else s

(* Combined normalize + truncate *)
let summarize ?(max_len = 100) s = normalize_whitespace s |> truncate max_len

(* Format count for margin display: "####" if 0, else padded number *)
let format_count count =
  if count > 0 then Format.sprintf "%4d" count else "####"

(* Calculate percentage *)
let percentage hit total =
  if total > 0 then 100.0 *. float hit /. float total else 0.0
