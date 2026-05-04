(** Shared CLI error/diagnostic plumbing.

    {!guard} runs a pipeline thunk inside a fresh diagnostic context, renders
    any diagnostics (and errors) to stderr, and exits with status 1 on [Error].
    Call sites stay free of exit plumbing. *)

val guard :
  color:Cli_args.color ->
  on_ok:('a -> unit) ->
  (unit -> ('a, Spectec.Error.t) result) ->
  unit

val guard_unit :
  color:Cli_args.color -> (unit -> (unit, Spectec.Error.t) result) -> unit
