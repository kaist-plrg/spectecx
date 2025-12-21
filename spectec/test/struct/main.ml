(** Structuring test - verifies spec structuring from IL to SL *)

open Core
open Test_lib

let run specdir =
  let open Core.Result.Let_syntax in
  let spec_sl =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Runner.parse_spec_files spec_files in
    let%bind spec_il = Runner.elaborate spec in
    let spec_sl = Runner.structure spec_il in
    Ok spec_sl
  in
  match spec_sl with
  | Error err ->
      Format.printf "Structuring failed:\n  %s\n"
        (Runner.Error.string_of_error err)
  | Ok spec_sl -> Format.printf "%s\n" (Lang.Sl.Print.string_of_spec spec_sl)

let command =
  Command.basic ~summary:"run structuring test"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"DIR spec directory" in
     fun () -> run specdir)

let () = Command_unix.run command
