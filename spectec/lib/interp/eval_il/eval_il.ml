open Lang.Il
module F = Format
open Attempt
open Common.Source

exception StepLimitExceeded = Error.StepLimitExceeded

let run_relation (filename : string) (builtins : Builtins.t) (cache : Cache.t)
    (spec : spec) (rid : id') (values : value list) : Ctx.t * value list =
  let ctx = Interp.load_spec filename builtins cache spec in
  let+ ctx, values = Interp.invoke_rel ctx (rid $ no_region) values in
  (ctx, values)

let run_relation_fresh (filename : string) (builtins : Builtins.t)
    (cache : Cache.t) (spec : spec) (rid : id') (values : value list) :
    Ctx.t * value list =
  Cache.clear cache;
  run_relation filename builtins cache spec rid values

type error = region * string

let run ?(max_steps = -1) (module T : Target.S) (spec : spec) (rid : string)
    (values : value list) (filename : string) : (Ctx.t * value list, error) result =
  let builtins = Builtins.make T.builtins in
  let cache =
    Cache.make ~is_impure_func:T.is_impure_func ~is_impure_rel:T.is_impure_rel
      ~state_version:T.state_version
  in
  let inner () =
    Interp.step_budget := max_steps;
    run_relation_fresh filename builtins cache spec rid values |> Result.ok
  in
  try T.handler inner with Error.InterpError (at, msg) -> Error (at, msg)

let error_to_string = Error.to_string

let error_to_diagnostic ((at, msg) : error) : Diagnostic.t =
  Diagnostic.error ~source:"il-interp" at msg

module Ctx = Ctx
