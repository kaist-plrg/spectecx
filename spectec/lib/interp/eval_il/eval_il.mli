open Common.Source

module Ctx : sig
  type t
end

type error = region * string

exception StepLimitExceeded

val error_to_string : error -> string
val error_to_diagnostic : error -> Diagnostic.t

val step_hook : (unit -> unit) ref

val run :
  (module Target.S) ->
  Lang.Il.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (Ctx.t * Lang.Il.Value.t list, error) result
