module Location = Common.Location

type error = Location.t * string

val error_to_diagnostic : error -> Diag.t

type context

val context_of_spec :
  ?check_determinism:bool ->
  (module Target.S) ->
  Lang.Pl.spec ->
  (context, error) result

val eval_rel :
  context ->
  rel:string ->
  values:Lang.Il.Value.t list ->
  (Lang.Il.Value.t list, error) result
