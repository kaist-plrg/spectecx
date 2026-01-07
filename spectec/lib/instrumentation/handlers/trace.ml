(* Trace handler - Live logging of interpreter events.

   Implements Instrumentation_core.Handler.S interface.

   Output levels:
   - Summary: relation/function enter/exit
   - Full: + rule/clauses, premises and iteration summaries

   Usage:
     let handler = Trace.make { level = Full; output = Instrumentation_core.Output.stdout }
*)

module Il = Lang.Il
open Instrumentation_core.Util

(* Verbosity levels *)
type level = Summary | Full

(* Handler configuration *)
type config = { level : level; output : Instrumentation_core.Output.t }

let default_config =
  { level = Summary; output = Instrumentation_core.Output.stdout }

let config = ref default_config
let fmt = ref Format.std_formatter

let summarize_value ?(max_len = 100) (value : Il.Value.t) : string =
  Il.Print.string_of_value value |> summarize ~max_len

let format_values (values : Il.Value.t list) : string =
  match values with
  | [] -> ""
  | _ ->
      let value_strs = List.map (summarize_value ~max_len:100) values in
      Format.sprintf "  [in: %s]\n" (String.concat ", " value_strs)

(* Runtime state - changes during execution *)
module State = struct
  let depth = ref 0
  let reset () = depth := 0

  let indent () =
    Format.sprintf "[%2d] %s" !depth (String.make (!depth * 2) ' ')
end

module M : Instrumentation_core.Handler.S = struct
  let init ~spec:_ = State.reset ()
  let on_instr = Instrumentation_core.Noop.on_instr
  let on_prem_exit = Instrumentation_core.Noop.on_prem_exit
  let finish = Instrumentation_core.Noop.finish

  let on_rel_enter ~id ~at:_ ~values =
    Format.fprintf !fmt "%s→ %s\n%!" (State.indent ()) id;
    if !config.level = Full && values <> [] then
      Format.fprintf !fmt "%s%s%!" (State.indent ()) (format_values values);
    incr State.depth

  let on_rel_exit ~id ~at:_ ~success =
    decr State.depth;
    Format.fprintf !fmt "%s← %s [%s]\n%!" (State.indent ()) id
      (if success then "ok" else "fail")

  let on_rule_enter ~id ~rule_id ~at:_ =
    if !config.level = Full then
      Format.fprintf !fmt "%s→ %s/%s\n%!" (State.indent ()) id rule_id

  let on_rule_exit ~id ~rule_id ~at:_ ~success =
    if !config.level = Full then
      Format.fprintf !fmt "%s← %s/%s [%s]\n%!" (State.indent ()) id rule_id
        (if success then "ok" else "fail")

  let on_func_enter ~id ~at:_ ~values =
    Format.fprintf !fmt "%s→ $%s\n%!" (State.indent ()) id;
    if !config.level = Full && values <> [] then
      Format.fprintf !fmt "%s%s%!" (State.indent ()) (format_values values);
    incr State.depth

  let on_func_exit ~id ~at:_ =
    decr State.depth;
    Format.fprintf !fmt "%s← %s\n%!" (State.indent ()) id

  let on_clause_enter ~id ~clause_idx ~at:_ =
    if !config.level = Full then
      Format.fprintf !fmt "%s→ $%s/%d\n%!" (State.indent ()) id clause_idx

  let on_clause_exit ~id ~clause_idx:_ ~at:_ ~success:_ =
    if !config.level = Full then
      Format.fprintf !fmt "%s← $%s\n%!" (State.indent ()) id

  let on_iter_prem_enter ~prem:_ ~at:_ =
    if !config.level = Full then
      Format.fprintf !fmt "%s  → [iteration]\n" (State.indent ())

  let on_iter_prem_exit ~at:_ =
    if !config.level = Full then
      Format.fprintf !fmt "%s  ← [iteration]\n%!" (State.indent ())

  let on_prem_enter ~prem ~at:_ =
    if !config.level = Full then
      Format.fprintf !fmt "%s  | -- %s\n%!" (State.indent ())
        (Il.Print.string_of_prem prem |> normalize_whitespace)
end

let make cfg =
  config := cfg;
  fmt := Instrumentation_core.Output.formatter cfg.output;
  (module M : Instrumentation_core.Handler.S)
