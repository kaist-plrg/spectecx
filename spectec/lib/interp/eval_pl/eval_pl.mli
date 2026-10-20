open Common.Source

module Ctx : sig
  type t
end

type error = region * string

val error_to_diagnostic : error -> Diag.t

(** With [check_determinism=true], [run] reports an error when multiple arms of
    a try instruction apply. [check_determinism] defaults to [false]. *)
val run :
  ?check_determinism:bool ->
  (module Target.S) ->
  Lang.Pl.spec ->
  string ->
  Lang.Il.Value.t list ->
  (Ctx.t * Lang.Il.Value.t list, error) result
