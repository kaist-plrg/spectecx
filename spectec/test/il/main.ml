(** IL interpreter test - runs P4 typechecker using the IL interpreter *)

open Core
open Test_lib

let run ~negative specdir includes exclude_dirs testdir =
  let open Core.Result.Let_syntax in
  let suite_result =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Runner.parse_spec_files spec_files in
    let%bind spec_il = Runner.elaborate spec in
    let filenames = Files.collect ~suffix:".p4" testdir in
    let exclude_set = Exclude.load exclude_dirs in
    let config : Suite.config =
      {
        name = "il typing";
        intro = "Running typing test on";
        heading = "typing test";
        success = "Typecheck success";
        failure = "Typecheck failed";
        expected_failure = "Expected typing failure";
        unexpected_success = "Unexpected typing success";
      }
    in
    let expectation =
      if negative then Runner.Task.Negative else Runner.Task.Positive
    in
    let run filename =
      Runner.Handlers.il (fun () ->
          let%bind _ =
            Runner.eval_il_p4_typechecker spec_il includes filename
          in
          Ok ())
    in
    Suite.run ~config ~exclude_set ~filenames ~expectation ~run;
    Ok ()
  in
  match suite_result with
  | Ok () -> ()
  | Error err ->
      Format.printf "Failed to run IL interpreter:\n  %s\n"
        (Runner.Error.string_of_error err)

let command =
  Command.basic ~summary:"run IL interpreter typing test"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"DIR spec directory"
     and includes = flag "-i" (listed string) ~doc:"DIR include paths"
     and exclude_dirs = flag "-e" (listed string) ~doc:"DIR exclude paths"
     and testdir = flag "-d" (required string) ~doc:"DIR test directory"
     and negative =
       flag "-neg" no_arg ~doc:" expect failures (negative mode)"
     in
     fun () -> run ~negative specdir includes exclude_dirs testdir)

let () = Command_unix.run command
