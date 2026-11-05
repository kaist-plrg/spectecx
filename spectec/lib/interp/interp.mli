module Builtins = Builtins
module Opaque_types = Opaque_types
module Value_typecheck = Value_typecheck
module Target = Target

type target_state
type error

val error_to_diagnostic : error -> Diag.t

(** [with_target_state target f] runs [f] with fresh target state and a [vid]
    counter starting at zero. Values built during [f] have distinct [vid]
    fields. The supplied [target_state] is valid only while [f] runs. *)
val with_target_state : (module Target.S) -> (target_state -> 'a) -> 'a

(** Relation calls from one context share its cache. A context is valid only
    within the [with_target_state] call that supplied its target state. All
    context constructors default [check_determinism] to [false]. With
    [check_determinism=true], the interpreter rejects multiple successful
    alternatives, even when outputs agree. *)
type context

val context_of_il_spec :
  ?check_determinism:bool ->
  target_state ->
  Lang.Il.spec ->
  (context, error) result

val context_of_sl_spec :
  ?check_determinism:bool ->
  target_state ->
  Lang.Sl.spec ->
  (context, error) result

val context_of_pl_spec :
  ?check_determinism:bool ->
  target_state ->
  Lang.Pl.spec ->
  (context, error) result

val eval_rel :
  context ->
  rel:string ->
  values:Lang.Il.Value.t list ->
  (Lang.Il.Value.t list, error) result
