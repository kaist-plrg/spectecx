module Builtins = Builtins
module Target = Target

type error
type ctx_il
type ctx_sl

exception StepLimitExceeded

val error_to_string : error -> string
val error_to_diagnostic : error -> Diagnostic.t

val eval_il :
  (module Target.S) ->
  Lang.Il.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (ctx_il * Lang.Il.Value.t list, error) result

val eval_sl :
  (module Target.S) ->
  Lang.Sl.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (ctx_sl * Lang.Il.Value.t list, error) result

val run_prems :
  ?max_steps:int ->
  (module Target.S) ->
  Lang.Il.spec ->
  (Lang.Il.id' * Lang.Il.Value.t) list ->
  Lang.Il.prem list ->
  string ->
  ((Lang.Il.id' * Lang.Il.Value.t) list, error) result
