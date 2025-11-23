open Types

type _ Effect.t += FreshVid : (unit -> vid) Effect.t
type _ Effect.t += FreshTid : (unit -> string) Effect.t
type _ Effect.t += ValueCreated : value -> unit Effect.t
