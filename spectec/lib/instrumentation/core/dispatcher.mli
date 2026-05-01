(** Event runtime for instrumentation handlers. Drive instrumentation via
    {!Instrumentation.with_instrumentation}; interpreters call {!emit}. [init]
    fails if instrumentation is already active; [finish] is idempotent. *)

val init : spec:Handler.spec -> handlers:(module Handler.S) list -> unit

(** Dispatch an event to all active handlers. No-op when no session is active,
    so interpreters can be driven without instrumentation configured. *)
val emit : Handler.event -> unit

val finish : unit -> unit
