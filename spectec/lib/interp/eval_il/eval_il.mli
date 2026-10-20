open Common.Source

module Ctx : sig
  type t
end

type error =
  | Plain of region * string
  | Backtrack of Common.Attempt.failtrace list

val error_to_diagnostic : error -> Diag.t

(** With [check_determinism=true], [run] reports a relation or function that
    applies through more than one rule or clause as an error.
    [check_determinism] defaults to [false]. *)
val run :
  ?check_determinism:bool ->
  (module Target.S) ->
  Lang.Il.spec ->
  string ->
  Lang.Il.Value.t list ->
  (Ctx.t * Lang.Il.Value.t list, error) result
