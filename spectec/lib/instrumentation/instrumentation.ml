module Dispatcher = Instrumentation_core.Dispatcher
module Event = Instrumentation_core.Event
module Output = Instrumentation_core.Output
module Util = Instrumentation_core.Util
module Static = Instrumentation_static.Static
module Premise_uid = Instrumentation_static.Premise_uid
module Branch_coverage = Instrumentation_handlers.Branch_coverage
module Node_coverage_il = Instrumentation_handlers.Node_coverage_il
module Node_coverage_sl = Instrumentation_handlers.Node_coverage_sl
module Profile = Instrumentation_handlers.Profile
module Trace = Instrumentation_handlers.Trace

module Handler = struct
  include Instrumentation_core.Handler
  module Spec = Instrumentation_core.Spec
  module Config = Instrumentation_core.Config
end

module Config = struct
  type t = Handler.Config.t list

  let default = Config.default
  let register_static_dependencies = Config.register_static_dependencies
  let handlers = Config.handlers
  let validate_mode = Config.validate_mode
  let close_outputs = Config.close_outputs
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
  let handlers = Config.handlers config in
  Static.reset_all ();
  Config.register_static_dependencies config;
  Static.init_all spec;
  Dispatcher.init ~spec ~handlers;
  let result = f () in
  Dispatcher.finish ();
  Config.close_outputs config;
  result
