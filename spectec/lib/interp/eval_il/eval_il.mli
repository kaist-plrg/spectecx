open Common.Source

module Ctx : sig
  type t
end

type error = region * string

val error_to_string : error -> string
val error_to_diagnostic : error -> Diagnostic.t

val run :
  (module Target.S) ->
  Lang.Il.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (Ctx.t * Lang.Il.Value.t list, error) result

val run_prems :
  (module Target.S) ->
  Lang.Il.spec ->
  (Lang.Il.id' * Lang.Il.Value.t) list ->
  Lang.Il.prem list ->
  Lang.Il.id' list ->
  string ->
  ((Lang.Il.id' * Lang.Il.Value.t) list, error) result
