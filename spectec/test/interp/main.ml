(** Interpreter test - Generic test runner for IL/SL using TASK *)

open Core
open Test_lib

(** Generic test runner - works with any TASK, supports IL or SL mode *)
let run_with_task (type i) (module T : Runner.Task.S with type input = i)
    ~sl_mode ~spec_files ~inputs ~exclude_dirs =
  let open Core.Result.Let_syntax in
  let suite_result =
    let%bind spec = Runner.parse_spec_files spec_files in
    let%bind spec_il = Runner.elaborate spec in
    let spec_sl = if sl_mode then Some (Runner.structure spec_il) else None in
    let exclude_set = Exclude.load exclude_dirs in
    let mode_suffix = if sl_mode then "(sl)" else "(il)" in
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
    (* Get filenames from inputs for Suite.run *)
    let filenames = List.map inputs ~f:(fun i -> T.source i) in
    (* Create a lookup from filename to input *)
    let input_table =
      List.fold inputs
        ~init:(Map.empty (module String))
        ~f:(fun acc input -> Map.set acc ~key:(T.source input) ~data:input)
    in
    let run filename =
      T.Target.handler (fun () ->
          match Map.find input_table filename with
          | None -> failwith ("T not found: " ^ filename)
          | Some input ->
              if sl_mode then
                let%bind _ =
                  Runner.eval_sl_with_task
                    (module T)
                    spec_il (Option.value_exn spec_sl) input
                in
                Ok ()
              else
                let%bind _ =
                  Runner.eval_il_with_task (module T) spec_il input
                in
                Ok ())
    in
    (* Use expectation from the first input, or default to Positive *)
    let expectation =
      match inputs with [] -> Runner.Task.Positive | i :: _ -> T.expectation i
    in
    Suite.run ~config ~exclude_set ~filenames ~expectation ~run;
    Ok ()
  in
  match suite_result with
  | Ok () -> ()
  | Error err ->
      let mode = if sl_mode then "SL" else "IL" in
      Format.printf "Failed to run %s interpreter:\n  %s\n" mode
        (Runner.Error.string_of_error err)

(** P4 Typecheck test - uses P4_Target.spec_dir *)
let run_p4_typecheck ~negative ~sl_mode ~includes ~exclude_dirs ~testdir =
  let expectation =
    if negative then Runner.Task.Negative else Runner.Task.Positive
  in
  (* Prefix for dune test which runs from spectec/_build/default/test/interp *)
  let repo_root = "../../../../../" in
  let spec_dir = repo_root ^ Targets_p4.P4.Target.spec_dir in
  let spec_files = Files.collect ~suffix:".spectec" spec_dir in
  (* Create inputs with our parameters *)
  let inputs =
    Targets_p4.P4.Typecheck.collect ~dir:testdir ()
    |> List.map ~f:(fun input ->
           {
             Targets_p4.P4.Typecheck.includes;
             filename = Targets_p4.P4.Typecheck.source input;
             expect = expectation;
           })
  in
  run_with_task
    (module Targets_p4.P4.Typecheck)
    ~sl_mode ~spec_files ~inputs ~exclude_dirs

let command =
  Command.basic ~summary:"run interpreter typing test (IL or SL)"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"DIR include paths"
     and exclude_dirs = flag "-e" (listed string) ~doc:"DIR exclude paths"
     and testdir = flag "-d" (required string) ~doc:"DIR test directory"
     and negative = flag "-neg" no_arg ~doc:" expect failures (negative mode)"
     and sl_mode =
       flag "--sl" no_arg ~doc:" use SL interpreter (default: IL)"
     in
     fun () ->
       run_p4_typecheck ~negative ~sl_mode ~includes ~exclude_dirs ~testdir)

let () = Command_unix.run command
