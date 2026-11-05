(** Subcommand constructors for a target's CLI command group.

    {!make_task}, {!make_debug}, {!make_parse}, {!make_batch}, and
    {!make_checkpoint} each build one labeled subcommand the caller composes
    into a {!Core.Command.group}.

    Each constructor takes the target as a first-class module so spec loading
    happens inside the callback — [--spec] overrides and the instrumentation
    lifecycle then behave correctly. *)

val make_task :
  (module Spectec.Target.S) ->
  name:string ->
  summary:string ->
  (module Task_cli.S) ->
  string * Core.Command.t

(** [make_debug target ~name tasks] builds a group with one command per
    [(name, task)], each running a single input of that task on the IL
    interpreter under the interactive debugger. *)
val make_debug :
  (module Spectec.Target.S) ->
  name:string ->
  (string * (module Task_cli.S)) list ->
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
