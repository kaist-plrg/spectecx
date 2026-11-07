module Location = Common.Location

type t =
  | PassError of Pass.error
  | InterpError of Interp.error
  | UnhandledException of string
  | TaskParseError of Location.t * string
  | TaskError of Location.t * string
  | RoundtripError of Location.t * string
  | SpecMismatchError of string * string
  | DirectoryError of string
  | ConfigError of Location.t * string

let to_diagnostics = function
  | PassError e -> Pass.error_to_diagnostics e
  | InterpError e -> Diag.Bag.singleton (Interp.error_to_diagnostic e)
  | UnhandledException msg ->
      Diag.Bag.singleton
        (Diag.error ~source:"internal" Common.Location.Unknown
           ("Unhandled exception: " ^ msg))
  | TaskParseError (at, msg) ->
      Diag.Bag.singleton (Diag.error ~source:"task-parse" at msg)
  | TaskError (at, msg) -> Diag.Bag.singleton (Diag.error ~source:"task" at msg)
  | RoundtripError (at, msg) ->
      Diag.Bag.singleton (Diag.error ~source:"roundtrip" at msg)
  | SpecMismatchError (expected, actual) ->
      Diag.Bag.singleton
        (Diag.error ~source:"config" Common.Location.Unknown
           (Printf.sprintf
              "Spec version mismatch: expected spec hash %s but got %s."
              expected actual))
  | DirectoryError msg ->
      Diag.Bag.singleton
        (Diag.error ~source:"config" Common.Location.Unknown msg)
  | ConfigError (at, msg) ->
      Diag.Bag.singleton (Diag.error ~source:"config" at msg)
