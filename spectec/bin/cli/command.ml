(** CLI command generator for input specs and target specs *)

(** Extended input spec with CLI argument parsing support *)
module type CLI_TASK = sig
  include Runner.Task.TASK

  (** Command-line argument parser that produces an input value *)
  val cli_flags : input Core.Command.Param.t
end

(* Collect spec files from a directory - I/O utility *)
let collect_spec_files spec_dir =
  Sys.readdir spec_dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".spectec")
  |> List.map (Filename.concat spec_dir)

(* Print outcome for a single test *)
let print_outcome (type i) (module T : Runner.Task.TASK with type input = i)
    source outcome =
  let open Runner in
  match outcome with
  | Task.Pass values ->
      Format.printf "Passed: %s\n  %s\n\n" source (T.format_output values)
  | Task.ExpectedFail err ->
      Format.printf "Expected fail (passed): %s\n  %s\n\n" source
        (Error.string_of_error err)
  | Task.Fail err ->
      Format.printf "Failed: %s\n  %s\n\n" source (Error.string_of_error err)
  | Task.UnexpectedPass values ->
      Format.printf "Unexpected pass (failed): %s\n  %s\n\n" source
        (T.format_output values)

(* Run interpreter on a single input and print result *)
let run_single (type i) (module T : Runner.Task.TASK with type input = i)
    ~config ~sl_mode ~spec_il (input : i) =
  let outcome =
    Runner.run_with_outcome (module T) ~config ~sl_mode ~spec_il input
  in
  print_outcome (module T) (T.source input) outcome

(* Run interpreter on a suite of inputs and print results *)
let run_suite (type i) (module T : Runner.Task.TASK with type input = i) ~config
    ~sl_mode ~spec_il (inputs : i list) =
  let results =
    Runner.run_suite_with_outcomes (module T) ~config ~sl_mode ~spec_il inputs
  in
  List.iter
    (fun Runner.{ source; outcome; _ } ->
      Format.printf ">>> Running %s on %s\n" T.name source;
      print_outcome (module T) source outcome)
    results;
  let summary = Runner.summarize_outcomes results in
  let passed = Runner.summary_passed summary in
  let failed = Runner.summary_failed summary in
  Format.printf "\nTest Results: %d/%d passed, %d failed\n" passed summary.total
    failed

(* Generate a CLI command for any CLI_TASK *)
let make (type i) ~summary (module T : CLI_TASK with type input = i) =
  Core.Command.basic ~summary
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("spec files" %: string))
     and sl_mode = flag "--sl" no_arg ~doc:" use SL interpreter (default: IL)"
     and suite_dir =
       flag "--suite" (optional string) ~doc:"DIR run on test suite"
     and input = T.cli_flags
     and config = Cli_args.config_flags in
     fun () ->
       let open Runner in
       let run () =
         let* spec = parse_spec_files filenames_spec in
         let* spec_il = elaborate spec in
         match suite_dir with
         | None ->
             run_single (module T) ~config ~sl_mode ~spec_il input;
             Ok ()
         | Some dir ->
             run_suite (module T) ~config ~sl_mode ~spec_il (T.collect dir);
             Ok ()
       in
       match run () with
       | Ok () -> ()
       | Error e ->
           Format.printf "Error:\n  %s\n" (Runner.Error.string_of_error e))

(* --- TARGET based commands --- *)

(* Generate command that runs a specific input spec from a target *)
let make_run_input (type i) (module T : Runner.Task.TASK with type input = i)
    ~spec_dir =
  Core.Command.basic ~summary:("Run " ^ T.name)
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map sl_mode =
       flag "--sl" no_arg ~doc:" use SL interpreter (default: IL)"
     and suite_dir =
       flag "--suite" (optional string) ~doc:"DIR run on test suite"
     and config = Cli_args.config_flags in
     fun () ->
       let open Runner in
       let run () =
         let spec_files = collect_spec_files spec_dir in
         let* spec = parse_spec_files spec_files in
         let* spec_il = elaborate spec in
         let test_dir = Option.value suite_dir ~default:spec_dir in
         let inputs = T.collect test_dir in
         run_suite (module T) ~config ~sl_mode ~spec_il inputs;
         Ok ()
       in
       match run () with
       | Ok () -> ()
       | Error e ->
           Format.printf "Error:\n  %s\n" (Runner.Error.string_of_error e))

(* Generate "run" subcommand group from TARGET *)
let make_run (module Tgt : Runner.Target.TARGET) =
  let subcommands =
    List.map
      (fun (Runner.Task.Pack (module T)) ->
        (T.name, make_run_input (module T) ~spec_dir:Tgt.spec_dir))
      Tgt.tasks
  in
  Core.Command.group ~summary:("Run " ^ Tgt.name ^ " interpreter") subcommands

(* Generate "coverage" command from TARGET - runs all input specs *)
let make_coverage (module Tgt : Runner.Target.TARGET) =
  Core.Command.basic
    ~summary:("Run coverage for all " ^ Tgt.name ^ " input specs")
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map sl_mode =
       flag "--sl" no_arg ~doc:" use SL interpreter (default: IL)"
     and config = Cli_args.config_flags in
     fun () ->
       let open Runner in
       let run () =
         let spec_files = collect_spec_files Tgt.spec_dir in
         let* spec = parse_spec_files spec_files in
         let* spec_il = elaborate spec in
         let results =
           run_target_coverage (module Tgt) ~config ~sl_mode spec_il
         in
         (* Print summary for each input spec *)
         List.iter
           (fun { task_name; summary } ->
             let passed = Runner.summary_passed summary in
             let failed = Runner.summary_failed summary in
             Format.printf "%s: %d/%d passed, %d failed\n" task_name passed
               summary.total failed)
           results;
         Ok ()
       in
       match run () with
       | Ok () -> ()
       | Error e ->
           Format.printf "Error:\n  %s\n" (Runner.Error.string_of_error e))
