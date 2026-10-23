module Builtins = Builtins
module Opaque_types = Opaque_types
module Value_typecheck = Value_typecheck
module Target = Target

type target_state
type error
type ctx_il
type ctx_sl
type ctx_pl

val error_to_diagnostic : error -> Diag.t

(** [with_target_state target f] runs [f] with fresh target state. The supplied
    [target_state] is valid only while [f] runs. *)
val with_target_state : (module Target.S) -> (target_state -> 'a) -> 'a

(** With [check_determinism=true], the interpreter rejects multiple successful
    alternatives, even when their outputs agree. By default, the interpreter
    returns the first successful alternative. *)

val eval_il :
  ?check_determinism:bool ->
  target_state ->
  Lang.Il.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (ctx_il * Lang.Il.Value.t list, error) result

val eval_sl :
  ?check_determinism:bool ->
  target_state ->
  Lang.Sl.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (ctx_sl * Lang.Il.Value.t list, error) result

val eval_pl :
  ?check_determinism:bool ->
  target_state ->
  Lang.Pl.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (ctx_pl * Lang.Il.Value.t list, error) result
