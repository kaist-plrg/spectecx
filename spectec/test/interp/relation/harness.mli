type compiled_spec = {
  filename : string;
  spec_il : Lang.Il.spec;
  spec_sl : Lang.Sl.spec;
  spec_pl : Lang.Pl.spec;
}

type mode = Il | Sl | Pl
type expectation

module Pure_target : Interp.Target.S

(** [compile_file filename] compiles [filename] into the IL, SL, and PL forms
    used by [check]. Any diagnostic emitted during compilation fails the test.
*)
val compile_file : string -> compiled_spec

(** [returns expected] requires evaluation to return values equal to those
    constructed by [expected]. *)
val returns : (unit -> Lang.Il.Value.t list) -> expectation

(** [fails] accepts any evaluation error. *)
val fails : expectation

(** [fails_with substring] accepts an evaluation error when its rendered
    diagnostic contains [substring]. *)
val fails_with : string -> expectation

(** [check spec ~name ~relation ~args expectation] evaluates [relation] in every
    mode in [modes], which contains all three modes by default. [check] creates
    fresh target state for each mode and runs [args], evaluation, and any
    expected-value callback within that state. [target] supplies builtins and
    defaults to a target with no builtins. With [check_determinism=true], the
    interpreter rejects multiple successful alternatives, even when their
    outputs agree. [check_determinism] defaults to [false]. *)
val check :
  compiled_spec ->
  ?target:(module Interp.Target.S) ->
  ?modes:mode list ->
  ?check_determinism:bool ->
  name:string ->
  relation:string ->
  args:(unit -> Lang.Il.Value.t list) ->
  expectation ->
  unit
