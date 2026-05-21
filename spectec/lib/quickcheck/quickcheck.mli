module Arbitrary = Arbitrary
module Gen = Gen
module Il_gen = Il_gen

type error = NoManualGenerator of string
type 'a result = ('a, error) Stdlib.result

(** A manually-implemented generator for a [builtin generator $id : t]
    declaration. Keyed by the EL identifier name. *)
type manual_gen = Lang.Il.spec -> (Lang.Il.id' * Lang.Il.Value.t) list Gen.t

val quickcheck_spec :
  generalize:bool ->
  max_steps:int ->
  num_tests:int ->
  save:bool ->
  manual_gens:(string * manual_gen) list ->
  Lang.Il.spec ->
  Qc_il.spec ->
  unit result

val error_to_string : error -> string
val error_to_diagnostic : error -> Diag.t
