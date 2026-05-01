(** Instrumentation facade. Drive instrumentation via {!with_instrumentation}
    rather than poking {!Dispatcher} directly. *)

module Dispatcher = Instrumentation_core.Dispatcher
module Output = Instrumentation_core.Output
module Util = Instrumentation_core.Util
module Static = Instrumentation_static.Static
module Premise_uid = Instrumentation_static.Premise_uid
module Branch_coverage = Instrumentation_handlers.Branch_coverage
module Node_coverage_il = Instrumentation_handlers.Node_coverage_il
module Node_coverage_sl = Instrumentation_handlers.Node_coverage_sl
module Profile = Instrumentation_handlers.Profile
module Trace = Instrumentation_handlers.Trace
module Config = Config

module Handler : sig
  include module type of Instrumentation_core.Handler
  module Spec = Instrumentation_core.Descriptor
end

(** Every built-in handler spec. Add a new handler by appending here and nowhere
    else — the CLI discovers handlers through this list. *)
val builtin_handler_specs : Handler.Spec.t list

(** [with_instrumentation config spec f] initializes static analyses, starts the
    instrumentation dispatcher, runs [f ()], and tears everything down via
    [Fun.protect] so outputs close and handlers finish on both the success and
    exception paths. *)
val with_instrumentation : Config.t -> Static.spec -> (unit -> 'a) -> 'a
