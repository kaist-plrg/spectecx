open Core
open Test_lib

(** Generic test runner - works with any TASK, in any interpreter mode *)
let run_with_task (type i) (module T : Spectec.Task.S with type input = i)
    ~(request : Spectec.Interp_mode.request) ~spec_files ~inputs ~exclude_set =
  let mode_name = Spectec.Interp_mode.to_string request in
  let open Core.Result.Let_syntax in
  let suite_result =
    let%bind spec = Spectec.parse_spec_files spec_files in
    let%bind spec_il, spec_al = Spectec.elaborate_and_algorithmize spec in
    let mode : Spectec.Interp_mode.t =
      match request with
      | `AL -> Al
      | `SL -> Sl
      | `PL ->
          Pl
            (Spectec.henv_load_il_spec
               (Spectec.henv_of_el_spec spec)
               ~spec:spec_il)
    in
    let mode_suffix = "(" ^ String.lowercase mode_name ^ ")" in
    let config : Suite.config =
      {
        name = T.name ^ " " ^ mode_suffix |> String.capitalize;
        intro = "Running " ^ T.name ^ " test on";
        heading = T.name ^ " test";
        success = T.name ^ " success" |> String.capitalize;
        failure = T.name ^ " failed" |> String.capitalize;
        expected_failure = "Expected " ^ T.name ^ " failure";
        unexpected_success = "Unexpected " ^ T.name ^ " success";
      }
    in
    let filenames = List.map inputs ~f:(fun i -> T.source i) in
    let input_table =
      List.fold inputs
        ~init:(Map.empty (module String))
        ~f:(fun acc input -> Map.set acc ~key:(T.source input) ~data:input)
    in
    let run filename =
      match Map.find input_table filename with
      | None -> failwith ("T not found: " ^ filename)
      | Some input ->
          let%bind _ =
            Spectec.eval_task_with_instrumentation
              (module T)
              ~mode ~spec_al input
          in
          Ok ()
    in
    let expectation =
      match inputs with
      | [] -> Spectec.Task.Positive
      | i :: _ -> T.expectation i
    in
    Suite.run ~config ~exclude_set ~filenames ~expectation ~run;
    Ok ()
  in
  match suite_result with
  | Ok () -> ()
  | Error err ->
      Format.printf "Failed to run %s interpreter:\n%s\n" mode_name
        (Spectec.Diagnostic.Render.render_bag
           ~ansi:Spectec.Diagnostic.Ansi.plain
           (Spectec.Error.to_diagnostics err))

let resolve_spec_dir dir =
  if Filename.is_relative dir then Filename.concat "../../../../../" dir
  else dir

(** P4 typecheck test over the spec at [Targets_p4.P4.Target.spec_dir] *)
let run_p4_typecheck ~negative ~request ~includes ~exclude_dirs ~testdirs =
  let expectation =
    if negative then Spectec.Task.Negative else Spectec.Task.Positive
  in
  let spec_dir = resolve_spec_dir Targets_p4.P4.Target.spec_dir in
  let spec_files = Files.collect ~suffix:".spectec" spec_dir in
  let inputs =
    List.concat_map testdirs ~f:(fun dir ->
        Targets_p4.P4.Typecheck.collect ~dir ())
    |> List.map ~f:(fun input ->
           {
             Targets_p4.P4.Typecheck.includes;
             filename = Targets_p4.P4.Typecheck.source input;
             expect = expectation;
           })
  in
  run_with_task
    (module Targets_p4.P4.Typecheck)
    ~request ~spec_files ~inputs
    ~exclude_set:(Exclude.load exclude_dirs)

(** Run each eBPF program under [testdirs] with an STF file of the same name. *)
let run_p4_eval ~request ~includes ~exclude_dirs ~testdirs =
  let module Eval = Targets_p4.P4.Eval in
  let spec_dir = resolve_spec_dir Targets_p4.P4.Target.spec_dir in
  let spec_files = Files.collect ~suffix:".spectec" spec_dir in
  let inputs =
    List.concat_map testdirs ~f:(fun dir -> Eval.collect ~dir ())
    |> List.map ~f:(fun (input : Eval.input) -> { input with includes })
  in
  (* Exclusion files name P4 programs or STF files. The suite names each run
     by its program, so each match must use the program's name. *)
  let excluded = Exclude.load exclude_dirs in
  let exclude_set =
    List.filter_map inputs ~f:(fun (input : Eval.input) ->
        Option.some_if
          (Exclude.mem excluded input.filename || Exclude.mem excluded input.stf)
          (Eval.source input))
    |> Exclude.Excludes.of_list
  in
  run_with_task (module Eval) ~request ~spec_files ~inputs ~exclude_set

(** Impty Typecheck test - per-variant spec dispatch *)
let run_impty_typecheck ~variant ~negative ~request ~testdirs =
  let expectation =
    if negative then Spectec.Task.Negative else Spectec.Task.Positive
  in
  let repo_root = "../../../../../" in
  let spec_dir = repo_root ^ "spectec/specs/impty/" ^ variant in
  let spec_files = Files.collect ~suffix:".spectec" spec_dir in
  let inputs =
    List.concat_map testdirs ~f:(fun dir ->
        Targets_impty.Impty.Typecheck.collect ~dir ())
    |> List.filter ~f:(fun input ->
           match
             (Targets_impty.Impty.Typecheck.expectation input, expectation)
           with
           | Spectec.Task.Positive, Spectec.Task.Positive -> true
           | Spectec.Task.Negative, Spectec.Task.Negative -> true
           | _ -> false)
  in
  run_with_task
    (module Targets_impty.Impty.Typecheck)
    ~request ~spec_files ~inputs ~exclude_set:Exclude.empty

let run_miniml_eval ~negative ~request ~testdirs =
  let expectation =
    if negative then Spectec.Task.Negative else Spectec.Task.Positive
  in
  let spec_dir = resolve_spec_dir Targets_miniml.Miniml.Target.spec_dir in
  let spec_files = Files.collect ~suffix:".spectec" spec_dir in
  let inputs =
    List.concat_map testdirs ~f:(fun dir ->
        Targets_miniml.Miniml.Eval.collect ~dir ())
    |> List.filter ~f:(fun input ->
           let source = Targets_miniml.Miniml.Eval.source input in
           let matches_expectation =
             match
               (Targets_miniml.Miniml.Eval.expectation input, expectation)
             with
             | Spectec.Task.Positive, Spectec.Task.Positive
             | Spectec.Task.Negative, Spectec.Task.Negative ->
                 true
             | _ -> false
           in
           let terminates =
             not (String.equal (Filename.basename source) "omega.ml")
           in
           terminates && matches_expectation)
  in
  run_with_task
    (module Targets_miniml.Miniml.Eval)
    ~request ~spec_files ~inputs ~exclude_set:Exclude.empty

let includes_flag =
  Command.Param.(flag "-i" (listed string) ~doc:"DIR include paths")

let exclude_dirs_flag =
  Command.Param.(flag "-e" (listed string) ~doc:"DIR exclude paths")

let testdirs_flag =
  Command.Param.(
    flag "-d" (listed string) ~doc:"DIR test directory (repeatable)")

let negative_flag =
  Command.Param.(flag "-neg" no_arg ~doc:" expect failures (negative mode)")

let test_command ~summary task_flags =
  Command.basic ~summary
  @@
  let open Command.Let_syntax in
  let%map run = task_flags and request = Cli.Cli_args.Interpreter.mode_flag in
  fun () -> run ~request

let p4_typecheck_command =
  test_command ~summary:"typecheck P4 programs"
  @@
  let open Command.Let_syntax in
  let%map includes = includes_flag
  and exclude_dirs = exclude_dirs_flag
  and testdirs = testdirs_flag
  and negative = negative_flag in
  run_p4_typecheck ~negative ~includes ~exclude_dirs ~testdirs

let p4_eval_command =
  test_command ~summary:"run P4 programs on their STF packets"
  @@
  let open Command.Let_syntax in
  let%map includes = includes_flag
  and exclude_dirs = exclude_dirs_flag
  and testdirs = testdirs_flag in
  run_p4_eval ~includes ~exclude_dirs ~testdirs

let impty_typecheck_command =
  test_command ~summary:"typecheck impty programs"
  @@
  let open Command.Let_syntax in
  let open Command.Param in
  let%map variant =
    flag "--variant"
      (required
         (Arg_type.of_alist_exn
            [
              ("base", "base");
              ("closure", "closure");
              ("recursion", "recursion");
            ]))
      ~doc:"VARIANT impty variant"
  and testdirs = testdirs_flag
  and negative = negative_flag in
  run_impty_typecheck ~variant ~negative ~testdirs

let miniml_eval_command =
  test_command ~summary:"evaluate Mini-ML programs"
  @@
  let open Command.Let_syntax in
  let%map testdirs = testdirs_flag and negative = negative_flag in
  run_miniml_eval ~negative ~testdirs

let command =
  Command.group ~summary:"run interpreter test (AL, SL, or PL)"
    [
      ( "p4",
        Command.group ~summary:"P4 tests"
          [ ("typecheck", p4_typecheck_command); ("eval", p4_eval_command) ] );
      ( "impty",
        Command.group ~summary:"impty tests"
          [ ("typecheck", impty_typecheck_command) ] );
      ( "miniml",
        Command.group ~summary:"Mini-ML tests" [ ("eval", miniml_eval_command) ]
      );
    ]

let () = Command_unix.run command
