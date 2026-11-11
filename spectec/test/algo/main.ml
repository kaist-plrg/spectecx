open Core
open Test_lib

let run specdir =
  let open Core.Result.Let_syntax in
  let spec_al =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Spectec.parse_spec_files spec_files in
    let%bind _spec_il, spec_al = Spectec.elaborate_and_algorithmize spec in
    Ok spec_al
  in
  match spec_al with
  | Ok spec_al -> Format.printf "%s\n" (Lang.Al.Print.string_of_spec spec_al)
  | Error err ->
      Format.printf "Elaboration or algorithmization failed:\n%s\n"
        (Spectec.Diagnostic.Render.render_bag
           ~ansi:Spectec.Diagnostic.Ansi.plain
           (Spectec.Error.to_diagnostics err));
      exit 1

let command =
  Command.basic ~summary:"run algo test"
  @@
  let open Command.Let_syntax in
  let open Command.Param in
  let%map specdir = flag "-s" (required string) ~doc:"DIR spec directory" in
  fun () -> run specdir

let () = Command_unix.run command
