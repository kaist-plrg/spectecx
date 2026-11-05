module Location = Common.Location

type error =
  | Plain of Location.t * string
  | Backtrack of Common.Attempt.failtrace list

val error_to_diagnostic : error -> Diag.t

type context

val context_of_spec :
  ?check_determinism:bool ->
  (module Target.S) ->
  Lang.Il.spec ->
  (context, error) result

val eval_rel :
  context ->
  rel:string ->
  values:Lang.Il.Value.t list ->
  (Lang.Il.Value.t list, error) result
