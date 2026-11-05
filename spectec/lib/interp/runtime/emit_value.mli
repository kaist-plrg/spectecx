(** Emit {!Instrumentation_api.Value_event.t} events to value handlers. Each
    function does nothing when no value handler subscribes. The functions that
    take [value] return it unchanged. *)

type value = Lang.Il.Value.t

val derived : value -> source:value -> value
val constructed : value -> components:value list -> value
val combined : value -> sources:value list -> value
val updated : value -> base:value -> replacement:value -> value

val invoked :
  callee:Instrumentation_api.Value_event.callee ->
  inputs:value list ->
  outputs:value list ->
  cached:bool ->
  unit
