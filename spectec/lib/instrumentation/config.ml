(* Instrumentation configuration.

   Consolidates all instrumentation options into a single record type.
   Each handler has its own config type with level (if applicable) and output.
   Use `to_handlers` to convert a config to the handler list for dispatcher. *)

module Trace = Instrumentation_handlers.Trace
module Profile = Instrumentation_handlers.Profile
module Branch_coverage = Instrumentation_handlers.Branch_coverage
module Node_coverage_il = Instrumentation_handlers.Node_coverage_il
module Node_coverage_sl = Instrumentation_handlers.Node_coverage_sl
module Output = Instrumentation_core.Output

type t = {
  trace : Trace.config option;
  profile : Profile.config option;
  branch_coverage : Branch_coverage.config option;
  node_coverage : Node_coverage_il.config option;
      (* shared by IL/SL - they're mutually exclusive at runtime *)
}

let default =
  { trace = None; profile = None; branch_coverage = None; node_coverage = None }

(* Convert config to handler list *)
let to_handlers config =
  (match config.trace with None -> [] | Some cfg -> [ Trace.make cfg ])
  @ (match config.profile with None -> [] | Some cfg -> [ Profile.make cfg ])
  @ (match config.branch_coverage with
    | None -> []
    | Some cfg -> [ Branch_coverage.make cfg ])
  @
  match config.node_coverage with
  | None -> []
  | Some cfg ->
      (* Both IL and SL handlers share the same config;
         they self-select based on spec type at init() *)
      [ Node_coverage_il.make cfg; Node_coverage_sl.make cfg ]

(* Close all output destinations after finish() *)
let close_outputs config =
  Option.iter (fun c -> Output.close c.Trace.output) config.trace;
  Option.iter (fun c -> Output.close c.Profile.output) config.profile;
  Option.iter
    (fun c -> Output.close c.Branch_coverage.output)
    config.branch_coverage;
  Option.iter
    (fun c -> Output.close c.Node_coverage_il.output)
    config.node_coverage
