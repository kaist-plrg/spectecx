(** Event runtime for instrumentation handlers. Drive instrumentation via
    {!Instrumentation.with_instrumentation}. Interpreters call {!emit} and
    {!emit_value}, and decoders call {!emit_value}. [init] fails if
    instrumentation is already active. [finish] is idempotent. *)

val init :
  spec:Instrumentation_api.Handler.spec ->
  handlers:(module Instrumentation_api.Handler.S) list ->
  unit

(** Dispatch an event to all active handlers. No-op when no session is active,
    so interpreters can be driven without instrumentation configured. *)
val emit : Instrumentation_api.Event.t -> unit

(** Like {!emit}, but builds the event only when a session is active. For events
    whose payload is expensive to construct and read only by handlers. *)
val emit_on_demand : (unit -> Instrumentation_api.Event.t) -> unit

(** Whether any active handler has [Some f] for [handle_value]. Returns [false]
    when no session is active. *)
val has_value_handlers : unit -> bool

(** Pass a value event to subscribing handlers in configured order. Does nothing
    when no session is active. *)
val emit_value : Instrumentation_api.Value_event.t -> unit

val finish : unit -> unit
