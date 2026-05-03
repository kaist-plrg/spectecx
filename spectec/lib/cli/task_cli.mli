(** Task_cli.S — the CLI binding for a single task: the parser that turns
    command-line flags into the task's input value.

    A single Task_cli is reused across multiple {!Subcommand} constructors: the
    same task may appear in {!Subcommand.make_task}, {!Subcommand.make_parse},
    and inside a {!Subcommand.make_batch} task list. Subcommand naming and
    summaries are therefore decided per call, not baked in here. *)

module type S = sig
  module Task : Spectec.Task.S

  val flags : Task.input Core.Command.Param.t
end
