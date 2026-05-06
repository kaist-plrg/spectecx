(** Trace: live-logs interpreter events as they happen.

    Levels are a strict progression of verbosity:
    - [Summary]: relation/function enter/exit only.
    - [Rules]: above + each rule attempt's enter/exit (which rule fired, and
      whether it succeeded).
    - [Inputs]: above + input values on enter and output values on exit.
    - [Full]: above + premises, iteration markers, and clause events (the
      inner-loop noise needed for debugging non-terminating evaluations).

    All levels stream — events are printed as they happen, not buffered. *)

module Il = Lang.Il
open Util

type level = Summary | Rules | Inputs | Full

let rank = function Summary -> 0 | Rules -> 1 | Inputs -> 2 | Full -> 3
let at_least target current = rank current >= rank target

type config = { level : level; output : Instrumentation_api.Output.t }

let default_config =
  { level = Summary; output = Instrumentation_api.Output.stdout }

let config = ref default_config
let fmt = ref Format.std_formatter

let summarize_value (v : Il.Value.t) : string =
  Il.Print.string_of_value v |> summarize ~max_len:100

let format_values tag (values : Il.Value.t list) : string =
  match values with
  | [] -> ""
  | _ ->
      let strs = List.map summarize_value values in
      Format.sprintf "  [%s: %s]" tag (String.concat ", " strs)

let depth = ref 0
let reset () = depth := 0
let indent () = Format.sprintf "[%2d] %s" !depth (String.make (!depth * 2) ' ')

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []
  let init ~spec:_ = reset ()
  let finish () = ()

  let print_in values =
    if at_least Inputs !config.level then
      Format.fprintf !fmt "%s%!" (format_values "in" values)

  let print_out values =
    if at_least Inputs !config.level then
      Format.fprintf !fmt "%s%!" (format_values "out" values)

  let handle : Instrumentation_api.Event.t -> unit = function
    | Test_start _ | Test_end _ -> reset ()
    | Rel_enter { id; at = _; values } ->
        Format.fprintf !fmt "%s→ %s" (indent ()) id;
        print_in values;
        Format.fprintf !fmt "\n%!";
        incr depth
    | Rel_exit { id; at = _; success; values } ->
        decr depth;
        Format.fprintf !fmt "%s← %s [%s]" (indent ()) id
          (if success then "ok" else "fail");
        if success then print_out values;
        Format.fprintf !fmt "\n%!"
    | Rule_enter { id; rule_id; at = _ } ->
        if at_least Rules !config.level then
          Format.fprintf !fmt "%s→ %s/%s\n%!" (indent ()) id rule_id
    | Rule_exit { id; rule_id; at = _; success } ->
        if at_least Rules !config.level then
          Format.fprintf !fmt "%s← %s/%s [%s]\n%!" (indent ()) id rule_id
            (if success then "ok" else "fail")
    | Func_enter { id; at = _; values } ->
        Format.fprintf !fmt "%s→ $%s" (indent ()) id;
        print_in values;
        Format.fprintf !fmt "\n%!";
        incr depth
    | Func_exit { id; at = _; value } ->
        decr depth;
        Format.fprintf !fmt "%s← $%s" (indent ()) id;
        print_out (Option.to_list value);
        Format.fprintf !fmt "\n%!"
    | Clause_enter { id; clause_idx; at = _ } ->
        if at_least Full !config.level then
          Format.fprintf !fmt "%s→ $%s/%d\n%!" (indent ()) id clause_idx
    | Clause_exit { id; clause_idx = _; at = _; success = _ } ->
        if at_least Full !config.level then
          Format.fprintf !fmt "%s← $%s\n%!" (indent ()) id
    | Iter_prem_enter _ ->
        if at_least Full !config.level then
          Format.fprintf !fmt "%s  → [iteration]\n%!" (indent ())
    | Iter_prem_exit _ ->
        if at_least Full !config.level then
          Format.fprintf !fmt "%s  ← [iteration]\n%!" (indent ())
    | Prem_enter { prem; at = _ } ->
        if at_least Full !config.level then
          Format.fprintf !fmt "%s  | -- %s\n%!" (indent ())
            (Il.Print.string_of_prem prem |> normalize_whitespace)
    | Prem_exit _ -> ()
    | Instr _ -> ()
end

let make cfg =
  config := cfg;
  fmt := Instrumentation_api.Output.formatter cfg.output;
  (module M : Instrumentation_api.Handler.S)

module Spec : Instrumentation_spec.Spec.S = struct
  let name = "trace"
  let mode = `Both

  let params =
    [
      ("level", "LEVEL verbosity level: summary|rules|inputs|full");
      Instrumentation_spec.Param_utils.output_param;
    ]

  let parse_level = function
    | "summary" -> Summary
    | "rules" -> Rules
    | "inputs" -> Inputs
    | "full" -> Full
    | s ->
        failwith
          ("Invalid trace level: " ^ s
         ^ " (expected: summary|rules|inputs|full)")

  let parse alist =
    match Instrumentation_spec.Param_utils.get alist "level" with
    | None -> None
    | Some s ->
        let output =
          Instrumentation_spec.Param_utils.output_of
            (Instrumentation_spec.Param_utils.get alist "output")
        in
        Some
          {
            Instrumentation_config.Handler_config.name;
            mode;
            handler = make { level = parse_level s; output };
            output;
          }

  let checkpoint = None
end

let spec : Instrumentation_spec.Spec.t = (module Spec)
