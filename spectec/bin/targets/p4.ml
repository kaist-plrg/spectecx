(** P4 CLI command - Extends Targets_p4 with CLI flags *)
open Targets_p4.P4

(** CLI_TASK for P4 typechecker *)
module Cli_task : Cli.Command.CLI_TASK with type input = Typecheck.input =
struct
  include Typecheck

  let cli_flags =
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map includes = flag "-i" (listed string) ~doc:"DIR P4 include paths"
    and filename = flag "-p" (required string) ~doc:"FILE P4 file to process" in
    make ~includes ~filename ()
end

(** P4 command *)
let command = Cli.Command.make ~summary:"Run P4 typechecker" (module Cli_task)
