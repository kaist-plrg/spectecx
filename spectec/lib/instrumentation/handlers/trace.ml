(** Trace: live-logs interpreter events as they happen. [Summary] covers
    relation and function enter/exit; [Full] additionally logs rules, clauses,
    premises, and iteration summaries. *)

module Il = Lang.Il
open Instrumentation_core.Util

type level = Summary | Full
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
  let static_dependencies = []
  let init ~spec:_ = State.reset ()
  let finish () = ()

  let handle : Instrumentation_core.Handler.event -> unit = function
    | Test_start _ | Test_end _ -> ()
    | Rel_enter { id; at = _; values } ->
        Format.fprintf !fmt "%s→ %s\n%!" (State.indent ()) id;
        if !config.level = Full && values <> [] then
          Format.fprintf !fmt "%s%s%!" (State.indent ()) (format_values values);
        incr State.depth
    | Rel_exit { id; at = _; success } ->
        decr State.depth;
        Format.fprintf !fmt "%s← %s [%s]\n%!" (State.indent ()) id
          (if success then "ok" else "fail")
    | Rule_enter { id; rule_id; at = _ } ->
        if !config.level = Full then
          Format.fprintf !fmt "%s→ %s/%s\n%!" (State.indent ()) id rule_id
    | Rule_exit { id; rule_id; at = _; success } ->
        if !config.level = Full then
          Format.fprintf !fmt "%s← %s/%s [%s]\n%!" (State.indent ()) id rule_id
            (if success then "ok" else "fail")
    | Func_enter { id; at = _; values } ->
        Format.fprintf !fmt "%s→ $%s\n%!" (State.indent ()) id;
        if !config.level = Full && values <> [] then
          Format.fprintf !fmt "%s%s%!" (State.indent ()) (format_values values);
        incr State.depth
    | Func_exit { id; at = _ } ->
        decr State.depth;
        Format.fprintf !fmt "%s← %s\n%!" (State.indent ()) id
    | Clause_enter { id; clause_idx; at = _ } ->
        if !config.level = Full then
          Format.fprintf !fmt "%s→ $%s/%d\n%!" (State.indent ()) id clause_idx
    | Clause_exit { id; clause_idx = _; at = _; success = _ } ->
        if !config.level = Full then
          Format.fprintf !fmt "%s← $%s\n%!" (State.indent ()) id
    | Iter_prem_enter { prem = _; at = _ } ->
        if !config.level = Full then
          Format.fprintf !fmt "%s  → [iteration]\n" (State.indent ())
    | Iter_prem_exit { at = _ } ->
        if !config.level = Full then
          Format.fprintf !fmt "%s  ← [iteration]\n%!" (State.indent ())
    | Prem_enter { prem; at = _ } ->
        if !config.level = Full then
          Format.fprintf !fmt "%s  | -- %s\n%!" (State.indent ())
            (Il.Print.string_of_prem prem |> normalize_whitespace)
    | Prem_exit _ -> ()
    | Instr _ -> ()
end

let make cfg =
  config := cfg;
  fmt := Instrumentation_core.Output.formatter cfg.output;
  (module M : Instrumentation_core.Handler.S)

module Descriptor : Instrumentation_core.Descriptor.S = struct
  let name = "trace"
  let mode = `Both

  let params =
    [
      Instrumentation_core.Param_utils.level_param;
      Instrumentation_core.Param_utils.output_param;
    ]

  let parse alist =
    match Instrumentation_core.Param_utils.get alist "level" with
    | None -> None
    | Some s ->
        let output =
          Instrumentation_core.Param_utils.output_of
            (Instrumentation_core.Param_utils.get alist "output")
        in
        let cfg =
          {
            level =
              Instrumentation_core.Param_utils.parse_level ~summary:Summary
                ~full:Full s;
            output;
          }
        in
        Some
          {
            Instrumentation_core.Descriptor.name;
            mode;
            handler = make cfg;
            output;
          }

  let checkpoint = None
end

let descriptor : Instrumentation_core.Descriptor.t = (module Descriptor)
