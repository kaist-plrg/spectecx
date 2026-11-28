(** Trace levels include the output from earlier levels in this list.

    - [Summary] shows the start and end of each call.
    - [Rules] adds rule attempts.
    - [Inputs] adds call arguments and results.
    - [Full] adds instructions, clauses and repetitions. *)

module Il = Lang.Il
module Al = Lang.Al
module Sl = Lang.Sl
module Pl = Lang.Pl
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

let printf_at_least target =
  if at_least target !config.level then Format.fprintf !fmt
  else Format.ifprintf !fmt

module State = struct
  let depth = ref 0

  (* Depth of each open repetition, innermost first. *)
  let repetition_depths : int list ref = ref []

  let reset () =
    depth := 0;
    repetition_depths := []

  let indent () =
    Format.sprintf "[%2d] %s" !depth (String.make (!depth * 2) ' ')

  let directly_in_repetition () =
    match !repetition_depths with
    | innermost :: _ -> innermost = !depth
    | [] -> false
end

let string_of_instr : Instrumentation_api.Event.instr -> string = function
  | AlInstr instr -> "-- " ^ Al.Print.string_of_instr instr
  | SlInstr instr -> Sl.Print.string_of_instr ~short:true instr
  | PlInstr instr -> Pl.Print.string_of_instr ~short:true instr

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []
  let init ~spec:_ = State.reset ()
  let finish () = ()

  (* One value per line, indented past the surrounding arrow. *)
  let print_values values =
    if at_least Inputs !config.level then
      let prefix = State.indent () ^ "    " in
      List.iter
        (fun v -> Format.fprintf !fmt "%s%s\n%!" prefix (summarize_value v))
        values

  let handle_value = None

  let handle : Instrumentation_api.Event.t -> unit = function
    | Test_start _ -> ()
    | Test_end _ -> State.reset ()
    | Rel_enter { id; at = _; inputs } ->
        Format.fprintf !fmt "%s→ %s\n%!" (State.indent ()) id;
        print_values inputs;
        incr State.depth
    | Rel_exit { id; at = _; success; conclusion } ->
        decr State.depth;
        Format.fprintf !fmt "%s← %s [%s]\n%!" (State.indent ()) id
          (if success then "ok" else "fail");
        if success then
          print_values (List.filter_map Fun.id (Il.Mode.outputs conclusion))
    | Rule_enter { id; rule_id; at = _; env = _ } ->
        printf_at_least Rules "%s→ %s/%s\n%!" (State.indent ()) id rule_id
    | Rule_exit { id; rule_id; at = _; success } ->
        printf_at_least Rules "%s← %s/%s [%s]\n%!" (State.indent ()) id rule_id
          (if success then "ok" else "fail")
    | Func_enter { id; at = _; inputs } ->
        Format.fprintf !fmt "%s→ $%s\n%!" (State.indent ()) id;
        print_values inputs;
        incr State.depth
    | Func_exit { id; at = _; output } ->
        decr State.depth;
        Format.fprintf !fmt "%s← $%s\n%!" (State.indent ()) id;
        print_values (Option.to_list output)
    | Clause_enter { id; clause_idx; at = _; env = _ } ->
        printf_at_least Full "%s→ $%s/%d\n%!" (State.indent ()) id clause_idx
    | Clause_exit { id; clause_idx = _; at = _; success = _ } ->
        printf_at_least Full "%s← $%s\n%!" (State.indent ()) id
    | Repetition_enter { index; count; at = _ } ->
        if at_least Full !config.level then (
          Format.fprintf !fmt "%s→ repetition %d of %d\n%!" (State.indent ())
            (index + 1) count;
          incr State.depth;
          State.repetition_depths := !State.depth :: !State.repetition_depths)
    | Repetition_exit { index; count; at = _; success } ->
        if at_least Full !config.level then (
          (State.repetition_depths :=
             match !State.repetition_depths with
             | _ :: outer -> outer
             | [] -> []);
          decr State.depth;
          Format.fprintf !fmt "%s← repetition %d of %d [%s]\n%!"
            (State.indent ()) (index + 1) count
            (if success then "ok" else "fail"))
    | Instr_enter { instr; at = _; env = _ } ->
        if at_least Full !config.level then
          (* The trace puts each instruction two columns past its repetition. *)
          let offset = if State.directly_in_repetition () then "" else "  " in
          Format.fprintf !fmt "%s%s| %s\n%!" (State.indent ()) offset
            (string_of_instr instr |> normalize_whitespace)
    | Instr_exit _ | Case_match _ -> ()
end

let make cfg =
  config := cfg;
  fmt := Instrumentation_api.Output.formatter cfg.output;
  (module M : Instrumentation_api.Handler.S)

module Spec : Instrumentation_spec.Spec.S = struct
  let name = "trace"
  let modes = [ `AL; `SL; `PL ]

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
            modes;
            handler = make { level = parse_level s; output };
            output;
          }

  let checkpoint = None
end

let spec : Instrumentation_spec.Spec.t = (module Spec)
