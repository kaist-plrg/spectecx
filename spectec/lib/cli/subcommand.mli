(** Subcommand constructors for a target's CLI command group.

    {!make_task}, {!make_parse}, {!make_batch}, and {!make_checkpoint} each
    build one labeled subcommand the caller composes into a
    {!Core.Command.group}.

    Each constructor takes the target as a first-class module so spec loading
    happens inside the callback — [--spec] overrides and the instrumentation
    lifecycle then behave correctly.

    {!with_error_handling} runs a pipeline thunk inside a fresh diagnostic
    context, renders any diagnostics (and errors) to stderr, and exits with
    status 1 on [Error]. Call sites stay free of exit plumbing. *)

val with_error_handling :
  color:Cli_args.color ->
  on_ok:('a -> unit) ->
  (unit -> ('a, Spectec.Error.t) result) ->
  unit

val with_error_handling_unit :
  color:Cli_args.color -> (unit -> (unit, Spectec.Error.t) result) -> unit

val make_task :
  (module Spectec.Target.S) ->
  name:string ->
  summary:string ->
  (module Task_cli.S) ->
  string * Core.Command.t

val make_parse :
  (module Spectec.Target.S) ->
  name:string ->
  summary:string ->
  (module Task_cli.S) ->
  string * Core.Command.t

val make_batch :
  (module Spectec.Target.S) ->
  name:string ->
  (module Task_cli.S) list ->
  string * Core.Command.t

val make_checkpoint :
  (module Spectec.Target.S) -> name:string -> string * Core.Command.t
