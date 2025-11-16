open Runner

let version = "0.1"

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       let elaborate_result =
         let* spec = parse_spec_files filenames in
         let* spec_il = elaborate spec in
         Ok spec_il
       in
       match elaborate_result with
       | Ok spec_il -> Format.printf "%s\n" (Il.Print.string_of_spec spec_il)
       | Error e -> Format.printf "%s\n" (Runner.Error.string_of_error e))

let structure_command =
  Core.Command.basic ~summary:"structure a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       let structure_result =
         let* spec = parse_spec_files filenames in
         let* spec_il = elaborate spec in
         let spec_sl = structure spec_il in
         Ok spec_sl
       in
       match structure_result with
       | Ok spec_sl -> Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl)
       | Error e -> Format.printf "%s\n" (Runner.Error.string_of_error e))

let p4parse_command =
  Core.Command.basic ~summary:"parse a P4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string))
     and includes_target = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_target =
       flag "-p" (required string) ~doc:"p4 file to typecheck"
     and roundtrip =
       flag "-r" no_arg ~doc:"perform a round-trip parse/unparse"
     in
     fun () ->
       let roundtrip_result =
         Runner.parse_p4_file_with_roundtrip roundtrip filenames includes_target
           filename_target
       in
       match (roundtrip, roundtrip_result) with
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

let run_il_command =
  Core.Command.basic ~summary:"run a spec based on backtracking IL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_target =
       flag "-i" (listed string) ~doc:"target file include paths"
     and filename_target =
       flag "-p" (required string) ~doc:"target file to run il interpreter on"
     and debug = flag "-dbg" no_arg ~doc:"print debug traces"
     and profile = flag "-profile" no_arg ~doc:"profiling" in
     fun () ->
       let interp_result =
         let* spec = parse_spec_files filenames_spec in
         let* spec_il = elaborate spec in
         let* _, _ =
           interp_il ~debug ~profile spec_il includes_target filename_target
         in
         Ok ()
       in
       match interp_result with
       | Ok () -> Format.printf "Interpreter succeeded\n"
       | Error e ->
           Format.printf "Interpreter failed:\n  %s\n"
             (Runner.Error.string_of_error e))

let run_sl_command =
  Core.Command.basic ~summary:"run a spec based on backtracking IL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_target =
       flag "-i" (listed string) ~doc:"target file include paths"
     and filename_target =
       flag "-p" (required string) ~doc:"target file to run il interpreter on"
     in
     fun () ->
       let interp_result =
         let* spec = parse_spec_files filenames_spec in
         let* spec_il = elaborate spec in
         let spec_sl = structure spec_il in
         let* _, _ = interp_sl spec_sl includes_target filename_target in
         Ok ()
       in
       match interp_result with
       | Ok () -> Format.printf "Interpreter succeeded\n"
       | Error e ->
           Format.printf "Interpreter failed:\n  %s\n"
             (Runner.Error.string_of_error e))

let command =
  Core.Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [
      ("elab", elab_command);
      ("struct", structure_command);
      ("run-sl", run_sl_command);
      ("run-il", run_il_command);
      ("p4parse", p4parse_command);
    ]

let () = Command_unix.run ~version command
