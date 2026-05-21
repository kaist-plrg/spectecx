type error = NoManualGenerator of string
type 'a result = ('a, error) Stdlib.result

val quickcheck_spec :
  generalize:bool ->
  max_steps:int ->
  num_tests:int ->
  save:bool ->
  Lang.Il.spec ->
  Qc_il.spec ->
  unit result

val error_to_string : error -> string
val error_to_diagnostic : error -> Diag.t
