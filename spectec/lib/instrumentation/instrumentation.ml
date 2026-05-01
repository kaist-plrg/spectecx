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

module Handler = struct
  include Instrumentation_core.Handler
  module Spec = Instrumentation_core.Descriptor
end

(* *** Add one entry here when adding a new handler *** *)
let builtin_handler_specs : Handler.Spec.t list =
  [
    Trace.spec;
    Profile.spec;
    Branch_coverage.spec;
    Node_coverage_il.spec;
    Node_coverage_sl.spec;
  ]

let with_instrumentation (config : Config.t) (spec : Static.spec)
    (f : unit -> 'a) : 'a =
  let handlers = Config.to_handlers config in
  Static.reset_all ();
  Static.init_all spec;
  Dispatcher.init ~spec ~handlers;
  let result = f () in
  Dispatcher.finish ();
  Config.close_outputs config;
  result
