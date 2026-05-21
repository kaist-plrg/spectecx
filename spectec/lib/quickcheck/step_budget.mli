(** Step budget for IL evaluation. [with_budget] installs an instrumentation
    handler that counts [Rel_enter] events and raises [StepLimitExceeded] when a
    bounded budget is exhausted. [max_steps = None] is unbounded; [Some n]
    permits [n] entries and raises on the next. *)

exception StepLimitExceeded

val with_budget : ?max_steps:int -> Lang.Il.spec -> (unit -> 'a) -> 'a
