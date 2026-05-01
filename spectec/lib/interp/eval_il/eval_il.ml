open Lang.Il
module F = Format
open Attempt
open Common.Source

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

let run (module T : Target.S) (spec : spec) (rid : string) (values : value list)
    (filename : string) : (Ctx.t * value list, error) result =
  let builtins = Builtins.make T.builtins in
  let cache =
    Cache.make ~is_impure_func:T.is_impure_func ~is_impure_rel:T.is_impure_rel
      ~state_version:T.state_version
  in
  let inner () =
    run_relation_fresh filename builtins cache spec rid values |> Result.ok
  in
  try T.handler inner with Error.InterpError (at, msg) -> Error (at, msg)

let error_to_string = Error.to_string

let error_to_diagnostic ((at, msg) : error) : Diagnostic.t =
  Diagnostic.error ~source:"il-interp" at msg

(* Evaluate a list of IL premises against an initial variable environment.
   Returns the updated bindings (initial + any newly bound by the premises).
   Caller supplies the names of all variables it wants back; variables not
   present in the context after evaluation are silently omitted. *)
let run_prems (module T : Target.S) (spec : spec)
    (initial_bindings : (id' * value) list) (prems : prem list)
    (all_var_names : id' list) (filename : string) :
    ((id' * value) list, error) result =
  let builtins = Builtins.make T.builtins in
  let cache =
    Cache.make ~is_impure_func:T.is_impure_func ~is_impure_rel:T.is_impure_rel
      ~state_version:T.state_version
  in
  let inner () =
    Cache.clear cache;
    let ctx = Interp.load_spec filename builtins cache spec in
    let ctx =
      List.fold_left
        (fun ctx (id, v) -> Ctx.add_value ctx (id $ no_region, []) v)
        ctx initial_bindings
    in
    let+ ctx = Interp.eval_prems ctx prems in
    let bindings =
      List.filter_map
        (fun id ->
          match Ctx.find_value_opt ctx (id $ no_region, []) with
          | Some v -> Some (id, v)
          | None -> None)
        all_var_names
    in
    Result.ok bindings
  in
  try T.handler inner with Error.InterpError (at, msg) -> Error (at, msg)

module Ctx = Ctx
