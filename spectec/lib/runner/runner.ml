open Lang
open Pass
open Interp
module Error = Error
module Task = Task
module Target = Target
module Checkpoint = Checkpoint

type 'a pipeline_result = ('a, Error.t) result

let ( let* ) = Result.bind

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
let eval_il_run (module T : Target.S) spec_il rid values_input filename_target :
    (Eval_Il.Ctx.t * Il.Value.t list) pipeline_result =
  let builtins = Builtins.make T.builtins in
  let run () =
    Eval_Il.Runner.run_relation_fresh filename_target builtins spec_il rid
      values_input
    |> Result.ok
  in
  try T.handler run
  with Eval_Il.Error.InterpError (at, msg) ->
    Error.IlInterpError (at, msg) |> Result.error

(* Core SL run function - no init/finish, used by both single and suite runners *)
let eval_sl_run (module T : Target.S) spec_sl rid values_input filename_target :
    (Eval_Sl.Ctx.t * Il.Value.t list) pipeline_result =
  let builtins = Builtins.make T.builtins in
  let run () =
    Eval_Sl.Runner.run_relation_fresh filename_target builtins spec_sl rid
      values_input
    |> Result.ok
  in
  try T.handler run
  with Eval_Sl.Error.InterpError (at, msg) ->
    Error.SlInterpError (at, msg) |> Result.error

(* Convert Static.spec to Handler.spec *)
let handler_spec_of_static = function
  | Instrumentation.Static.IlSpec s -> Instrumentation.Handler.IlSpec s
  | Instrumentation.Static.SlSpec s -> Instrumentation.Handler.SlSpec s

(* Shared init/finish lifecycle: register static deps, init handlers, run f, finish *)
let with_instrumentation config spec_type f =
  let handlers = Instrumentation.Config.to_handlers config in
  Instrumentation.Static.reset_all ();
  Instrumentation.Static.init_all spec_type;
  Instrumentation.Dispatcher.set_handlers handlers;
  Instrumentation.Dispatcher.init ~spec:(handler_spec_of_static spec_type);
  let result = f () in
  Instrumentation.Dispatcher.finish ();
  Instrumentation.Config.close_outputs config;
  result

(* Single-run wrappers that set up handlers, init, run, and finish *)
let eval_il (module T : Target.S) ?(config = Instrumentation.Config.default)
    spec_il rid values_input filename_target :
    (Eval_Il.Ctx.t * Il.Value.t list) pipeline_result =
  with_instrumentation config (Instrumentation.Static.IlSpec spec_il)
  @@ fun () -> eval_il_run (module T) spec_il rid values_input filename_target

let eval_sl (module T : Target.S) ?(config = Instrumentation.Config.default)
    spec_sl rid values_input filename_target :
    (Eval_Sl.Ctx.t * Il.Value.t list) pipeline_result =
  with_instrumentation config (Instrumentation.Static.SlSpec spec_sl)
  @@ fun () -> eval_sl_run (module T) spec_sl rid values_input filename_target

(* Single-run with input spec - includes full init/finish lifecycle *)
let eval_il_with_task (type input) (module T : Task.S with type input = input)
    ?(config = Instrumentation.Config.default) spec_il (input : input) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  eval_il (module T.Target) ~config spec_il relation values (T.source input)

let eval_sl_with_task (type input) (module T : Task.S with type input = input)
    ?(config = Instrumentation.Config.default) spec_il spec_sl (input : input) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  eval_sl (module T.Target) ~config spec_sl relation values (T.source input)

(* Run-only versions - no init/finish, for use in batch/coverage runs *)
let eval_il_with_task_run (type input)
    (module T : Task.S with type input = input) spec_il (input : input) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  eval_il_run (module T.Target) spec_il relation values (T.source input)

let eval_sl_with_task_run (type input)
    (module T : Task.S with type input = input) spec_il spec_sl (input : input)
    =
  let* relation, values = T.parse_input ~spec:spec_il input in
  eval_sl_run (module T.Target) spec_sl relation values (T.source input)

(* --- Higher-level runners using expectation and test_outcome --- *)

(* Run single input and compute outcome based on expectation.
   Includes full init/finish lifecycle - use for single runs. *)
let run_with_outcome (type i) (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il (input : i) =
  let result =
    T.Target.handler (fun () ->
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

(* Run single input without init/finish lifecycle.
   For use in batch/coverage runs where init/finish is managed externally. *)
let run_with_outcome_no_lifecycle (type i)
    (module T : Task.S with type input = i) ~sl_mode ~spec_il (input : i) =
  let test_case_id = T.source input in
  (* Notify handlers of test start *)
  Instrumentation.Dispatcher.notify_test_start ~test_case_id;
  let result =
    try
      T.Target.handler (fun () ->
          if sl_mode then
            let spec_sl = structure spec_il in
            let* _, values =
              eval_sl_with_task_run (module T) spec_il spec_sl input
            in
            Ok values
          else
            let* _, values = eval_il_with_task_run (module T) spec_il input in
            Ok values)
    with e ->
      (* Notify handlers of test end on exception *)
      Instrumentation.Dispatcher.notify_test_end ~test_case_id;
      raise e
  in
  (* Notify handlers of test end after execution *)
  Instrumentation.Dispatcher.notify_test_end ~test_case_id;
  Task.compute_outcome (T.expectation input) result

(* Result for a single test in a suite *)
type 'i test_result = {
  input : 'i;
  source : string;
  outcome : Task.test_outcome;
}

(* Run suite of inputs and return individual outcomes *)
let run_suite_with_outcomes (type i) (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il
    ?(verbose = false) (inputs : i list) =
  let total = List.length inputs in
  let run () =
    List.mapi
      (fun idx input ->
        let source = T.source input in
        if verbose then Format.printf "[%d/%d] %s... %!" (idx + 1) total source;
        let outcome =
          try run_with_outcome_no_lifecycle (module T) ~sl_mode ~spec_il input
          with exception_value ->
            let error =
              Error.IlInterpError
                (Common.Source.no_region, Printexc.to_string exception_value)
            in
            Task.compute_outcome (T.expectation input) (Error error)
        in
        (if verbose then
           match outcome with
           | Task.Pass _ -> Format.printf "PASS\n%!"
           | Task.ExpectedFail _ -> Format.printf "EXPECTED FAIL\n%!"
           | Task.Fail _ -> Format.printf "FAIL\n%!"
           | Task.UnexpectedPass _ -> Format.printf "UNEXPECTED PASS\n%!");
        { input; source; outcome })
      inputs
  in
  with_instrumentation config (Instrumentation.Static.IlSpec spec_il) run

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

(* Run coverage across all input specs in a target with checkpoint support.
   Init/finish lifecycle is managed here - called once for the entire run. *)
let run_target_coverage ?(config = Instrumentation.Config.default) ?test_dir
    ~(checkpoint_config : Checkpoint.config) ~verbose ~sl_mode ~spec_files
    spec_il tasks =
  let run_coverage () =
    (* Load checkpoint if resuming *)
    let loaded_checkpoint =
      match checkpoint_config.resume_from with
      | Some file -> (
          match Checkpoint.verify_and_load ~file ~spec_files ~verbose with
          | Ok checkpoint -> Some checkpoint
          | Error e ->
              Format.printf "%s\n" (Error.string_of_error e);
              None)
      | None -> None
    in

    (* Track completed inputs across all tasks *)
    let all_completed_inputs = ref [] in
    (match loaded_checkpoint with
    | Some checkpoint ->
        all_completed_inputs := checkpoint.Checkpoint.completed_inputs;
        Checkpoint.restore_coverage checkpoint
    | None -> ());

    let save_current_checkpoint () =
      Checkpoint.save ~spec_files ~completed_inputs:!all_completed_inputs
        ~output_file:checkpoint_config.output_file
    in

    let results =
      List.map
        (fun (Task.Pack (module T)) ->
          (* Each task discovers its own inputs *)
          let all_inputs =
            match test_dir with
            | Some dir -> T.collect ~dir ()
            | None -> T.collect ()
          in
          let total_all = List.length all_inputs in
          (* Filter out completed inputs if resuming *)
          let inputs =
            match loaded_checkpoint with
            | Some checkpoint ->
                Checkpoint.filter_remaining checkpoint all_inputs
                  ~get_id:T.source
            | None -> all_inputs
          in
          let completed_count = total_all - List.length inputs in
          if verbose then
            Format.printf "Running %s (%d tests, %d already completed)...\n%!"
              T.name (List.length inputs) completed_count;
          let task_results =
            List.mapi
              (fun index input ->
                let source = T.source input in
                if verbose then
                  (* Show absolute progress: [completed+1/total] *)
                  Format.printf "  [%d/%d] %s... %!"
                    (completed_count + index + 1)
                    total_all source;
                (* Use no_lifecycle version - init/finish managed at coverage level *)
                let outcome =
                  try
                    run_with_outcome_no_lifecycle
                      (module T)
                      ~sl_mode ~spec_il input
                  with exception_value ->
                    let error =
                      Error.IlInterpError
                        ( Common.Source.no_region,
                          Printexc.to_string exception_value )
                    in
                    Task.compute_outcome (T.expectation input) (Error error)
                in
                (if verbose then
                   match outcome with
                   | Task.Pass _ -> Format.printf "PASS\n%!"
                   | Task.ExpectedFail _ -> Format.printf "EXPECTED FAIL\n%!"
                   | Task.Fail _ -> Format.printf "FAIL\n%!"
                   | Task.UnexpectedPass _ ->
                       Format.printf "UNEXPECTED PASS\n%!");
                (* Track completion *)
                all_completed_inputs := source :: !all_completed_inputs;
                (* Periodic checkpoint save *)
                if (index + 1) mod checkpoint_config.save_interval = 0 then
                  save_current_checkpoint ();
                { input; source; outcome })
              inputs
          in
          { task_name = T.name; summary = summarize_outcomes task_results })
        tasks
    in
    (* Final checkpoint save before instrumentation finish *)
    save_current_checkpoint ();
    results
  in
  with_instrumentation config (Instrumentation.Static.IlSpec spec_il)
    run_coverage
