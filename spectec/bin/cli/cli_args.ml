(** Shared CLI argument parsers *)

(** Parse a 1/2 level argument into a typed level *)
let parse_level ~summary ~full = function
  | Some 1 -> Some summary
  | Some 2 -> Some full
  | _ -> None

(** Shared instrumentation config CLI flags *)
let config_flags =
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map trace =
    flag "--trace" (optional int)
      ~doc:"LEVEL trace verbosity: 0=off, 1=summary, 2=full"
  and profile = flag "--profile" no_arg ~doc:" print profiling info"
  and branch_coverage =
    flag "--branch-coverage" (optional int)
      ~doc:"LEVEL branch coverage: 1=summary, 2=full"
  and node_coverage =
    flag "--node-coverage" (optional int)
      ~doc:"LEVEL node coverage: 1=summary, 2=full"
  in
  Instrumentation.Config.
    {
      trace =
        parse_level ~summary:Instrumentation.Trace.Summary
          ~full:Instrumentation.Trace.Full trace;
      profile;
      branch_coverage =
        parse_level ~summary:Instrumentation.Branch_coverage.Summary
          ~full:Instrumentation.Branch_coverage.Full branch_coverage;
      node_coverage =
        parse_level ~summary:Instrumentation.Config.Summary
          ~full:Instrumentation.Config.Full node_coverage;
    }
