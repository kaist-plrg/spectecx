(** Instrumentation facade. Drive sessions via {!with_session} rather than
    poking {!Dispatcher} directly. *)

module Handler = Instrumentation_core.Handler
module Descriptor = Instrumentation_core.Descriptor
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

(** Every built-in handler descriptor. Add a new handler by appending here and
    nowhere else — the CLI discovers handlers through this list. *)
val all_descriptors : Descriptor.t list

(** [with_session config spec f] initializes static analyses, starts a
    dispatcher session, runs [f ()], and tears everything down via [Fun.protect]
    so outputs close and handlers finish on both the success and exception
    paths. *)
val with_session : Config.t -> Static.spec -> (unit -> 'a) -> 'a
