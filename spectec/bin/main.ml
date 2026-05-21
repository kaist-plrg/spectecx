open Spectec

let version = "0.1"
let ( let* ) = Result.bind

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_il ->
        Format.printf "%s\n" (Lang.Il.Print.string_of_spec spec_il))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* { lang; _ } = elaborate spec in
    Ok lang

let structure_command =
  Core.Command.basic ~summary:"structure a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_sl ->
        Format.printf "%s\n" (Lang.Sl.Print.string_of_spec spec_sl))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* { lang; _ } = elaborate spec in
    let spec_sl = structure lang in
    Ok spec_sl

let quickcheck_command =
  Core.Command.basic ~summary:"run quickcheck properties declared in a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and generalize =
    flag "--generalize" no_arg
      ~doc:" generalize counterexamples after shrinking"
  and max_steps =
    flag "--max-steps"
      (optional_with_default 100 int)
      ~doc:"N max steps per relation evaluation (default 100)"
  and num_tests =
    flag "--num-tests"
      (optional_with_default 100 int)
      ~doc:"N number of test cases to generate (default 100)"
  and save =
    flag "--save" no_arg ~doc:" save passing test inputs to {property}.json"
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard_unit ~color @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* { lang; qc } = elaborate spec in
    Quickcheck.quickcheck_spec ~generalize ~max_steps ~num_tests ~save lang qc
    |> Result.map_error (fun e ->
           Error.QuickcheckError (Quickcheck.error_to_string e))

let command =
  let module P4 = Targets_p4.P4.Cli in
  let module Impty = Targets_impty.Impty.Cli in
  Core.Command.group ~summary:"SpecTec command line tools"
    [
      ("elab", elab_command);
      ("struct", structure_command);
      (P4.name, P4.command);
      (Impty.name, Impty.command);
      ("quickcheck", quickcheck_command);
    ]

let () = Command_unix.run ~version command
