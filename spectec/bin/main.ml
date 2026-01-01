open Runner

let version = "0.1"

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("spec files" %: string)) in
     fun () ->
       let elaborate_result =
         let* spec = parse_spec_files filenames in
         let* spec_il = elaborate spec in
         Ok spec_il
       in
       match elaborate_result with
       | Ok spec_il ->
           Format.printf "%s\n" (Lang.Il.Print.string_of_spec spec_il)
       | Error e -> Format.printf "%s\n" (Runner.Error.string_of_error e))

let structure_command =
  Core.Command.basic ~summary:"structure a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("spec files" %: string)) in
     fun () ->
       let structure_result =
         let* spec = parse_spec_files filenames in
         let* spec_il = elaborate spec in
         let spec_sl = structure spec_il in
         Ok spec_sl
       in
       match structure_result with
       | Ok spec_sl ->
           Format.printf "%s\n" (Lang.Sl.Print.string_of_spec spec_sl)
       | Error e -> Format.printf "%s\n" (Runner.Error.string_of_error e))

let p4parse_command =
  Core.Command.basic ~summary:"parse a P4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("spec files" %: string))
     and includes_target = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_target = flag "-p" (required string) ~doc:"p4 file to parse"
     and roundtrip =
       flag "-r" no_arg ~doc:"perform a round-trip parse/unparse"
     in
     fun () ->
       let do_roundtrip () =
         let* rountrip_result =
           Runner.parse_p4_file_with_roundtrip roundtrip filenames
             includes_target filename_target
         in
         Ok rountrip_result
       in
       match (roundtrip, Runner.Handlers.il do_roundtrip) with
       | false, Ok unparsed_string ->
           Format.printf "Parse succeeded:\n%s\n" unparsed_string
       | true, Ok unparsed_string ->
           Format.printf "Roundtrip succeeded:\n%s\n" unparsed_string
       | false, Error e ->
           Format.printf "Parse failed:\n  %s\n"
             (Runner.Error.string_of_error e)
       | true, Error e ->
           Format.printf "Roundtrip failed:\n  %s\n"
             (Runner.Error.string_of_error e))

(* Helper to build instrumentation config from CLI options *)
let make_config ~trace ~profile ~branch_coverage ~node_coverage =
  let trace_level =
    match trace with
    | Some 1 -> Some Instrumentation.Trace.Summary
    | Some 2 -> Some Instrumentation.Trace.Full
    | _ -> None
  in
  let branch_level =
    match branch_coverage with
    | Some 1 -> Some Instrumentation.Branch_coverage.Summary
    | Some 2 -> Some Instrumentation.Branch_coverage.Full
    | _ -> None
  in
  let node_level =
    match node_coverage with
    | Some 1 -> Some Instrumentation.Config.Summary
    | Some 2 -> Some Instrumentation.Config.Full
    | _ -> None
  in
  Instrumentation.Config.
    {
      trace = trace_level;
      profile;
      branch_coverage = branch_level;
      node_coverage = node_level;
    }

let type_p4_il_command =
  Core.Command.basic
    ~summary:
      "typecheck a P4 program based on a SpecTec spec, using the IL interpreter"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("spec files" %: string))
     and includes_target = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_target =
       flag "-p" (required string) ~doc:"p4 file to typecheck"
     and trace =
       flag "--trace" (optional int)
         ~doc:
           "LEVEL trace verbosity: 0=off (default), 1=summary (call stack \
            only), 2=full (all details)"
     and profile = flag "--profile" no_arg ~doc:"print profiling info"
     and branch_coverage =
       flag "--branch-coverage" (optional int)
         ~doc:"LEVEL branch coverage: 1=summary, 2=full"
     and node_coverage =
       flag "--node-coverage" (optional int)
         ~doc:"LEVEL node coverage: 1=summary, 2=full"
     in
     fun () ->
       let config =
         make_config ~trace ~profile ~branch_coverage ~node_coverage
       in
       let interp () =
         let* spec = parse_spec_files filenames_spec in
         let* spec_il = elaborate spec in
         let* value_program = parse_p4_file includes_target filename_target in
         let* _, _ =
           eval_il ~config spec_il "Program_ok" [ value_program ]
             filename_target
         in
         Ok ()
       in
       match Runner.Handlers.il interp with
       | Ok () -> Format.printf "Interpreter succeeded\n"
       | Error e ->
           Format.printf "Interpreter failed:\n  %s\n"
             (Runner.Error.string_of_error e))

let type_p4_sl_command =
  Core.Command.basic
    ~summary:
      "typecheck a P4 program based on a SpecTec spec, using the SL interpreter"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("spec files" %: string))
     and includes_target = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_target =
       flag "-p" (required string) ~doc:"p4 file to typecheck"
     and trace =
       flag "--trace" (optional int)
         ~doc:
           "LEVEL trace verbosity: 0=off (default), 1=summary (call stack \
            only), 2=full (all details)"
     and profile = flag "--profile" no_arg ~doc:"print profiling info"
     and branch_coverage =
       flag "--branch-coverage" (optional int)
         ~doc:"LEVEL branch coverage: 1=summary, 2=full"
     and node_coverage =
       flag "--node-coverage" (optional int)
         ~doc:"LEVEL node coverage: 1=summary, 2=full"
     in
     fun () ->
       let config =
         make_config ~trace ~profile ~branch_coverage ~node_coverage
       in
       let interp () =
         let* spec = parse_spec_files filenames_spec in
         let* spec_il = elaborate spec in
         let spec_sl = structure spec_il in
         let* value_program = parse_p4_file includes_target filename_target in
         let* _, _ =
           eval_sl ~config spec_sl "Program_ok" [ value_program ]
             filename_target
         in
         Ok ()
       in
       match Runner.Handlers.sl interp with
       | Ok () -> Format.printf "Interpreter succeeded\n"
       | Error e ->
           Format.printf "Interpreter failed:\n  %s\n"
             (Runner.Error.string_of_error e))

(* Helper to collect files from directory *)
let collect_files ~suffix dir =
  let rec walk acc path =
    if Sys.is_directory path then
      Array.fold_left
        (fun acc name -> walk acc (Filename.concat path name))
        acc (Sys.readdir path)
    else if Filename.check_suffix path suffix then path :: acc
    else acc
  in
  walk [] dir |> List.sort String.compare

let coverage_p4_il_command =
  Core.Command.basic ~summary:"run IL interpreter coverage on a test suite"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("spec files" %: string))
     and includes_target = flag "-i" (listed string) ~doc:"DIR include paths"
     and testdir = flag "-d" (required string) ~doc:"DIR test directory"
     and branch_coverage =
       flag "--branch-coverage" (optional int)
         ~doc:"LEVEL branch coverage: 1=summary, 2=full"
     and node_coverage =
       flag "--node-coverage" (optional int)
         ~doc:"LEVEL node coverage: 1=summary, 2=full"
     in
     fun () ->
       let config =
         make_config ~trace:None ~profile:false ~branch_coverage ~node_coverage
       in
       let run () =
         let* spec = parse_spec_files filenames_spec in
         let* spec_il = elaborate spec in
         let filenames = collect_files ~suffix:".p4" testdir in
         let result =
           eval_il_suite_p4_typechecker ~config spec_il includes_target
             filenames
         in
         Ok result
       in
       match run () with
       | Ok { passed; failed; total } ->
           Format.printf "\nTest Results: %d/%d passed, %d failed\n" passed
             total failed
       | Error e ->
           Format.printf "Coverage suite failed:\n  %s\n"
             (Runner.Error.string_of_error e))

let coverage_p4_sl_command =
  Core.Command.basic ~summary:"run SL interpreter coverage on a test suite"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("spec files" %: string))
     and includes_target = flag "-i" (listed string) ~doc:"DIR include paths"
     and testdir = flag "-d" (required string) ~doc:"DIR test directory"
     and branch_coverage =
       flag "--branch-coverage" (optional int)
         ~doc:"LEVEL branch coverage: 1=summary, 2=full"
     and node_coverage =
       flag "--node-coverage" (optional int)
         ~doc:"LEVEL node coverage: 1=summary, 2=full"
     in
     fun () ->
       let config =
         make_config ~trace:None ~profile:false ~branch_coverage ~node_coverage
       in
       let run () =
         let* spec = parse_spec_files filenames_spec in
         let* spec_il = elaborate spec in
         let spec_sl = structure spec_il in
         let filenames = collect_files ~suffix:".p4" testdir in
         let result =
           eval_sl_suite_p4_typechecker ~config spec_il spec_sl includes_target
             filenames
         in
         Ok result
       in
       match run () with
       | Ok { passed; failed; total } ->
           Format.printf "\nTest Results: %d/%d passed, %d failed\n" passed
             total failed
       | Error e ->
           Format.printf "Coverage suite failed:\n  %s\n"
             (Runner.Error.string_of_error e))

let command =
  Core.Command.group ~summary:"SpecTec command line tools"
    [
      ("elab", elab_command);
      ("struct", structure_command);
      ("type-p4-il", type_p4_il_command);
      ("type-p4-sl", type_p4_sl_command);
      ("p4parse", p4parse_command);
      ("coverage-p4-il", coverage_p4_il_command);
      ("coverage-p4-sl", coverage_p4_sl_command);
    ]

let () = Command_unix.run ~version command
