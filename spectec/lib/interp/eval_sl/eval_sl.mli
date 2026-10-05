open Common.Source

module Ctx : sig
  type t
end

type error = region * string

val error_to_diagnostic : error -> Diag.t

(** With [check_determinism=true], [run] reports a block in which more than one
    alternative applies as an error. [check_determinism] defaults to [false]. *)
val run :
  ?check_determinism:bool ->
  (module Target.S) ->
  Lang.Sl.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (Ctx.t * Lang.Il.Value.t list, error) result
