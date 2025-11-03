open Util.Error

let version = "0.1"

exception ElabErrList of elaboration_error list

let elab_spec spec =
  match Elaborate.Elab.elab_spec spec with
  | Spec spec_il -> spec_il
  | Errors errors ->
      raise
        (ElabErrList
           (List.map (fun (at, failtraces) -> (at, failtraces)) errors))

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames in
         let spec_il = elab_spec spec in
         Format.printf "%s\n" (Il.Print_debug.string_of_spec spec_il);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabErrList errors ->
           Format.printf "%s\n" (string_of_elab_errors errors))

let parse_command =
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
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames in
         let spec_il = elab_spec spec in
         let value_program =
           Runner.parse_file includes_target filename_target
         in
         let unparsed_string =
           Format.asprintf "%a\n" (Concrete.Pp.pp_program spec_il) value_program
         in
         if roundtrip then
           let parsed_program =
             Runner.parse_string filename_target unparsed_string
           in
           Il.Eq.eq_value ~dbg:true value_program parsed_program
           |> (fun b ->
           if b then "Roundtrip successful" else "Roundtrip failed")
           |> print_endline
         else unparsed_string |> print_endline
       with
       | Util.Error.ParseError (_, msg) -> Format.printf "ill-formed: %s\n" msg
       | Util.Error.InterpError (_, msg) ->
           Format.printf "Interpreter failed: %s\n" msg
       | ElabErrList errors ->
           Format.printf "%s\n" (string_of_elab_errors errors))

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
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = elab_spec spec in
         let _, _ =
           Runner.interp_il ~debug ~profile spec_il includes_target
             filename_target
         in
         Format.printf "Interpreter succeeded\n"
       with
       | Util.Error.ParseError (_, msg) -> Format.printf "ill-formed: %s\n" msg
       | Util.Error.InterpError (_, msg) ->
           Format.printf "Interpreter failed: %s\n" msg
       | ElabErrList errors ->
           Format.printf "%s\n" (string_of_elab_errors errors))

let command =
  Core.Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [
      ("elab", elab_command);
      ("run-il", run_il_command);
      ("parse", parse_command);
    ]

let () = Command_unix.run ~version command
