(* Instrumentation - Top-level module re-exporting core and handlers.

   Core modules (from instrumentation_core):
   - Handler: Handler.S and Handler.S_with_data signatures
   - Dispatcher: Event dispatch functions
   - Noop: Default no-op handler
   - Output: Output destinations
   - Util: Shared utilities

   Handler implementations (from instrumentation_handlers):
   - Branch_coverage, Node_coverage_il, Node_coverage_sl, Profile, Trace

   Config module (local):
   - Config: Configuration type and handler factory
*)

(* Re-export core modules *)
module Handler = Instrumentation_core.Handler
module Dispatcher = Instrumentation_core.Dispatcher
module Noop = Instrumentation_core.Noop
module Output = Instrumentation_core.Output
module Util = Instrumentation_core.Util

(* Re-export static analysis modules *)
module Static = Instrumentation_static.Static
module Premise_uid = Instrumentation_static.Premise_uid

(* Re-export handler implementations *)
module Branch_coverage = Instrumentation_handlers.Branch_coverage
module Node_coverage_il = Instrumentation_handlers.Node_coverage_il
module Node_coverage_sl = Instrumentation_handlers.Node_coverage_sl
module Profile = Instrumentation_handlers.Profile
module Trace = Instrumentation_handlers.Trace

(* Config is defined locally in this library *)
module Config = Config
