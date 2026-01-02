open Lang
open Lang.Il
open Pass
open Interface
open Interp
module Error = Error
module Task = Task
module Target = Target

type 'a pipeline_result = ('a, Error.t) result

let ( let* ) = Result.bind

module Handlers = struct
  let il f =
    let vid_counter = ref 0 in
    let tid_counter = ref 0 in
    Effect.Deep.try_with f ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Effects.FreshVid ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let id = !vid_counter in
                    incr vid_counter;
                    Effect.Deep.continue k (fun () -> id))
            | Effects.FreshTid ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let tid = "FRESH__" ^ string_of_int !tid_counter in
                    incr tid_counter;
                    Effect.Deep.continue k (fun () -> tid))
            | Effects.ValueCreated _ ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    (* No-op *)
                    Effect.Deep.continue k ())
            | _ -> None (* Other effects *));
      }

  (* SL interpreter uses IL handler for now *)
  let sl = il
end

(* --- General runners --- *)

(* Transformations *)

let parse_spec_files filenames : El.spec pipeline_result =
  let parse_spec_files () =
    List.concat_map Frontend.Parse.parse_file filenames |> Result.ok
  in
  try parse_spec_files ()
  with Frontend.Error.ParseError (at, msg) ->
    Error.ParseError (at, msg) |> Result.error

let elaborate spec_el : Il.spec pipeline_result =
  let elaborate () =
    Elaborate.Elab.elab_spec spec_el
    |> Result.map_error (fun elab_err_list -> Error.ElabError elab_err_list)
  in
  try elaborate ()
  with Elaborate.Error.ElabError (at, failtraces) ->
    Error.ElabError [ (at, failtraces) ] |> Result.error

let structure spec_il : Sl.spec = Structure.Struct.struct_spec spec_il

(* Interpreters *)

(* Core IL run function - no init/finish, used by both single and suite runners *)
let eval_il_run spec_il rid values_input filename_target :
    (Eval_Il.Ctx.t * Il.Value.t list) pipeline_result =
  let run () =
    Eval_Il.Runner.run_relation_fresh spec_il rid values_input filename_target
    |> Result.ok
  in
  try Handlers.il run
  with Eval_Il.Error.InterpError (at, msg) ->
    Error.IlInterpError (at, msg) |> Result.error

(* Core SL run function - no init/finish, used by both single and suite runners *)
let eval_sl_run spec_sl rid values_input filename_target :
    (Eval_Sl.Ctx.t * Il.Value.t list) pipeline_result =
  let run () =
    Eval_Sl.Runner.run_relation_fresh spec_sl rid values_input filename_target
    |> Result.ok
  in
  try Handlers.sl run
  with Eval_Sl.Error.InterpError (at, msg) ->
    Error.SlInterpError (at, msg) |> Result.error

(* Single-run wrappers that set up handlers, init, run, and finish *)
let eval_il ?(config = Instrumentation.Config.default) spec_il rid values_input
    filename_target : (Eval_Il.Ctx.t * Il.Value.t list) pipeline_result =
  let handlers = Instrumentation.Config.to_handlers config in
  Instrumentation.Hooks.set_handlers handlers;
  Instrumentation.Hooks.init ~spec:(Instrumentation.Hooks.IlSpec spec_il);
  let result = eval_il_run spec_il rid values_input filename_target in
  Instrumentation.Hooks.finish ();
  result

let eval_sl ?(config = Instrumentation.Config.default) spec_sl rid values_input
    filename_target : (Eval_Sl.Ctx.t * Il.Value.t list) pipeline_result =
  let handlers = Instrumentation.Config.to_handlers config in
  Instrumentation.Hooks.set_handlers handlers;
  Instrumentation.Hooks.init ~spec:(Instrumentation.Hooks.SlSpec spec_sl);
  let result = eval_sl_run spec_sl rid values_input filename_target in
  Instrumentation.Hooks.finish ();
  result

(* Coverage suite runners - init once, run all files, finish once *)

type suite_result = { passed : int; failed : int; total : int }
type suite_input = (string * Il.Value.t list * string, Error.t) result

(* General IL suite runner - takes a list of result-wrapped inputs *)
let eval_il_suite ?(config = Instrumentation.Config.default) spec_il
    (inputs : suite_input list) : suite_result =
  let handlers = Instrumentation.Config.to_handlers config in
  Instrumentation.Hooks.set_handlers handlers;
  Instrumentation.Hooks.init ~spec:(Instrumentation.Hooks.IlSpec spec_il);
  let passed, failed =
    List.fold_left
      (fun (p, f) input ->
        match input with
        | Error _ -> (p, f + 1)
        | Ok (rid, values, filename) -> (
            let result = eval_il_run spec_il rid values filename in
            match result with Ok _ -> (p + 1, f) | Error _ -> (p, f + 1)))
      (0, 0) inputs
  in
  Instrumentation.Hooks.finish ();
  { passed; failed; total = List.length inputs }

(* General SL suite runner - takes a list of result-wrapped inputs *)
let eval_sl_suite ?(config = Instrumentation.Config.default) spec_sl
    (inputs : suite_input list) : suite_result =
  let handlers = Instrumentation.Config.to_handlers config in
  Instrumentation.Hooks.set_handlers handlers;
  Instrumentation.Hooks.init ~spec:(Instrumentation.Hooks.SlSpec spec_sl);
  let passed, failed =
    List.fold_left
      (fun (p, f) input ->
        match input with
        | Error _ -> (p, f + 1)
        | Ok (rid, values, filename) -> (
            let result = eval_sl_run spec_sl rid values filename in
            match result with Ok _ -> (p + 1, f) | Error _ -> (p, f + 1)))
      (0, 0) inputs
  in
  Instrumentation.Hooks.finish ();
  { passed; failed; total = List.length inputs }

(* --- T-spec-based runners --- *)

(* Single-run with input spec *)
let eval_il_with_task (type input)
    (module T : Task.TASK with type input = input)
    ?(config = Instrumentation.Config.default) spec_il (input : input) =
  let* relation, values = T.parse ~spec:spec_il input in
  eval_il ~config spec_il relation values (T.source input)

let eval_sl_with_task (type input)
    (module T : Task.TASK with type input = input)
    ?(config = Instrumentation.Config.default) spec_il spec_sl (input : input) =
  let* relation, values = T.parse ~spec:spec_il input in
  eval_sl ~config spec_sl relation values (T.source input)

(* Suite run with input spec *)
let eval_il_suite_with_task (type i) (module T : Task.TASK with type input = i)
    ?(config = Instrumentation.Config.default) spec_il (inputs : i list) =
  let suite_inputs =
    List.map
      (fun input ->
        T.parse ~spec:spec_il input
        |> Result.map (fun (rel, vals) -> (rel, vals, T.source input)))
      inputs
  in
  eval_il_suite ~config spec_il suite_inputs

let eval_sl_suite_with_task (type i) (module T : Task.TASK with type input = i)
    ?(config = Instrumentation.Config.default) spec_il spec_sl (inputs : i list)
    =
  let suite_inputs =
    List.map
      (fun input ->
        T.parse ~spec:spec_il input
        |> Result.map (fun (rel, vals) -> (rel, vals, T.source input)))
      inputs
  in
  eval_sl_suite ~config spec_sl suite_inputs

(* --- Higher-level runners using expectation and test_outcome --- *)

(* Run single input and compute outcome based on expectation *)
let run_with_outcome (type i) (module T : Task.TASK with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il (input : i) =
  let result =
    let handler = if sl_mode then Handlers.sl else Handlers.il in
    handler (fun () ->
        if sl_mode then
          let spec_sl = structure spec_il in
          let* _, values =
            eval_sl_with_task (module T) ~config spec_il spec_sl input
          in
          Ok values
        else
          let* _, values = eval_il_with_task (module T) ~config spec_il input in
          Ok values)
  in
  Task.compute_outcome (T.expectation input) result

(* Result for a single test in a suite *)
type 'i test_result = {
  input : 'i;
  source : string;
  outcome : Task.test_outcome;
}

(* Run suite of inputs and return individual outcomes *)
let run_suite_with_outcomes (type i) (module T : Task.TASK with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il
    (inputs : i list) =
  List.map
    (fun input ->
      let outcome =
        run_with_outcome (module T) ~config ~sl_mode ~spec_il input
      in
      { input; source = T.source input; outcome })
    inputs

(* Summary stats from suite results - tracks all four outcome types *)
type suite_summary = {
  pass : int; (* Positive test succeeded *)
  expected_fail : int; (* Negative test failed as expected *)
  fail : int; (* Positive test failed *)
  unexpected_pass : int; (* Negative test succeeded unexpectedly *)
  total : int;
}

(* Convenience getters *)
let summary_passed s = s.pass + s.expected_fail
let summary_failed s = s.fail + s.unexpected_pass

let summarize_outcomes results =
  let pass, expected_fail, fail, unexpected_pass =
    List.fold_left
      (fun (p, ef, f, up) { outcome; _ } ->
        match outcome with
        | Task.Pass _ -> (p + 1, ef, f, up)
        | Task.ExpectedFail _ -> (p, ef + 1, f, up)
        | Task.Fail _ -> (p, ef, f + 1, up)
        | Task.UnexpectedPass _ -> (p, ef, f, up + 1))
      (0, 0, 0, 0) results
  in
  { pass; expected_fail; fail; unexpected_pass; total = List.length results }

(* Result for one input spec in coverage run *)
type task_result = { task_name : string; summary : suite_summary }

(* Run coverage across all input specs in a target *)
let run_target_coverage (module Target : Target.TARGET)
    ?(config = Instrumentation.Config.default) ~sl_mode spec_il =
  List.map
    (fun (Task.Pack (module T)) ->
      let inputs = T.collect Target.spec_dir in
      let results =
        run_suite_with_outcomes (module T) ~config ~sl_mode ~spec_il inputs
      in
      { task_name = T.name; summary = summarize_outcomes results })
    Target.tasks

(* --- P4 runners --- *)

(* P4 Parsing *)

let parse_p4_file includes_p4 filename_p4 : Il.Value.t pipeline_result =
  let parse_p4_file () =
    P4.Parse.parse_file includes_p4 filename_p4 |> Result.ok
  in
  try Handlers.il parse_p4_file
  with P4.Error.P4ParseError (at, msg) ->
    Error.P4ParseError (at, msg) |> Result.error

let parse_p4_string filename_p4 string : Il.Value.t pipeline_result =
  let parse_p4_string () =
    P4.Parse.parse_string filename_p4 string |> Result.ok
  in
  try Handlers.il parse_p4_string
  with P4.Error.P4ParseError (at, msg) ->
    Error.P4ParseError (at, msg) |> Result.error

let parse_p4_file_with_roundtrip roundtrip filenames_spec includes_p4
    filename_p4 : string pipeline_result =
  let* spec_el = parse_spec_files filenames_spec in
  let* spec_il = elaborate spec_el in
  let* value_program = parse_p4_file includes_p4 filename_p4 in
  let unparsed_string =
    Format.asprintf "%a\n" (Concrete.Pp.pp_program spec_il) value_program
  in
  if roundtrip then
    let* value_program_rt = parse_p4_string filename_p4 unparsed_string in
    let eq = Il.Eq.eq_value ~dbg:true value_program value_program_rt in
    if eq then unparsed_string |> Result.ok
    else
      Error.RoundtripError (Common.Source.no_region, "Roundtrip failed")
      |> Result.error
  else unparsed_string |> Result.ok
