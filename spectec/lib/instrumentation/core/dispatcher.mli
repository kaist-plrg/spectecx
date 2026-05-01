(** Event dispatcher for instrumentation handlers. Drive sessions via
    {!Instrumentation.with_session}; interpreters call {!emit}. [init] fails if
    a session is already active; [finish] is idempotent. *)

val init : spec:Handler.spec -> handlers:(module Handler.S) list -> unit

(** Dispatch an event to all active handlers. No-op when no session is active,
    so interpreters can be driven without instrumentation configured. *)
val emit : Handler.event -> unit

val finish : unit -> unit
