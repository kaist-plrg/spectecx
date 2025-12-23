(* Trace handler - Live logging of interpreter events.

   Implements Hooks.HANDLER interface.
   Supports verbosity levels:
   - Summary: relation/function  enter/exit
   - Full: + rule/clauses, premises and iteration summaries

   Usage:
     let handler = Trace.make ~level:Full () in
     Hooks.set_handlers [handler];
     Hooks.finish ()
*)

module Il = Lang.Il

(* Verbosity levels *)
type level = Summary | Full

(* Normalize whitespace: collapse all whitespace to single spaces *)
let normalize_whitespace s =
  let buf = Buffer.create (String.length s) in
  let last_was_space = ref false in
  String.iter
    (fun c ->
      if c = ' ' || c = '\n' || c = '\t' || c = '\r' then (
        if not !last_was_space then Buffer.add_char buf ' ';
        last_was_space := true)
      else (
        Buffer.add_char buf c;
        last_was_space := false))
    s;
  Buffer.contents buf

(* Summarize a value - normalize whitespace and truncate *)
let summarize_value ?(max_len = 100) (value : Il.Value.t) : string =
  let full = Il.Print.string_of_value value |> normalize_whitespace in
  if String.length full <= max_len then full
  else String.sub full 0 (max_len - 3) ^ "..."

let format_values (values : Il.Value.t list) : string =
  match values with
  | [] -> ""
  | _ ->
      let value_strs = List.map (summarize_value ~max_len:100) values in
      Format.sprintf "  [in: %s]\n" (String.concat ", " value_strs)

module State = struct
  let depth = ref 0
  let level = ref Summary

  let reset ~level:l =
    depth := 0;
    level := l

  let indent () =
    Format.sprintf "[%2d] %s" !depth (String.make (!depth * 2) ' ')
end

module Handler : Hooks.HANDLER = struct
  let on_rel_enter ~id ~at:_ ~values =
    Format.printf "%s→ %s\n%!" (State.indent ()) id;
    (* Only print inputs in full mode *)
    if !State.level = Full && values <> [] then
      Format.printf "%s%s%!" (State.indent ()) (format_values values);
    incr State.depth

  let on_rel_exit ~id ~at:_ ~success =
    decr State.depth;
    Format.printf "%s← %s [%s]\n%!" (State.indent ()) id
      (if success then "ok" else "fail")

  let on_rule_enter ~id ~rule_id ~at:_ =
    if !State.level = Full then
      Format.printf "%s→ %s/%s\n%!" (State.indent ()) id rule_id

  let on_rule_exit ~id ~rule_id ~at:_ ~success =
    if !State.level = Full then
      Format.printf "%s← %s/%s [%s]\n%!" (State.indent ()) id rule_id
        (if success then "ok" else "fail")

  let on_func_enter ~id ~at:_ ~values =
    Format.printf "%s→ $%s\n%!" (State.indent ()) id;
    (* Only print inputs in full mode *)
    if !State.level = Full && values <> [] then
      Format.printf "%s%s%!" (State.indent ()) (format_values values);
    incr State.depth

  let on_func_exit ~id ~at:_ =
    decr State.depth;
    Format.printf "%s← %s\n%!" (State.indent ()) id

  let on_clause_enter ~id ~clause_idx ~at:_ =
    if !State.level = Full then
      Format.printf "%s→ $%s/%d\n%!" (State.indent ()) id clause_idx

  let on_clause_exit ~id ~at:_ =
    if !State.level = Full then Format.printf "%s← $%s\n%!" (State.indent ()) id

  (* Function invocation return - decrement depth *)

  (* TODO: incr/decr depth *)
  let on_iter_prem_enter ~prem:_ ~at:_ =
    if !State.level = Full then
      Format.printf "%s  → [iteration]\n" (State.indent ())

  let on_iter_prem_exit ~at:_ =
    if !State.level = Full then
      Format.printf "%s  ← [iteration]\n%!" (State.indent ())

  let on_prem ~prem ~at:_ =
    if !State.level = Full then
      Format.printf "%s  | -- %s\n%!" (State.indent ())
        (Il.Print.string_of_prem prem |> normalize_whitespace)

  let on_instr ~at:_ = ()
  let finish () = ()
end

let make ?(level = Summary) () : (module Hooks.HANDLER) =
  State.reset ~level;
  (module Handler)
