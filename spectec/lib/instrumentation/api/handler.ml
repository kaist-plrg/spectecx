(** Runtime contract for instrumentation handlers.

    A handler receives lifecycle and execution events from interpreters via
    {!Dispatcher}. Event constructors live in {!Event}; handlers are
    plugin-style and also declare themselves via {!Spec.S} for CLI parsing and
    construction. *)

type spec = Instrumentation_static.Static.spec =
  | IlSpec of Lang.Il.spec
  | SlSpec of Lang.Sl.spec

(** Base handler signature. Handlers pattern-match on {!Event.t} and only need
    to act on the constructors they care about (default: [| _ -> ()]). *)
module type S = sig
  (** Static analyses this handler needs. Registered via
      {!Instrumentation.Config.register_static_dependencies} before runtime
      init. *)
  val static_dependencies : (module Instrumentation_static.Static.S) list

  val init : spec:spec -> unit
  val handle : Event.t -> unit
  val finish : unit -> unit
end

(** Extends {!S} with structured access to collected data, for backends that
    need more than file/stdout output — e.g. checkpoint resume. *)
module type S_with_data = sig
  include S

  type result

  val get_result : unit -> result
  val restore : result -> unit
end
