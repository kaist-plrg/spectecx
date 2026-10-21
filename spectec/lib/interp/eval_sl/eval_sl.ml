open Common.Source
open Lang.Sl
open Error
module F = Format

let run_relation ~check_determinism ~opaque_types (builtins : Builtins.t)
    (cache : Interp_runtime.Cache.t) (spec : spec) (rid : id')
    (values : value list) : Ctx.t * value list =
  let ctx =
    Interp.context_of_spec spec ~builtins ~cache ~check_determinism
      ~opaque_types
  in
  let mode, _, _ = Ctx.find_rel_exn ctx ~id:(rid $ no_region) in
  let typs =
    List.map
      (fun (exp : Lang.Sl.exp) -> exp.note.typ $ exp.at)
      (Lang.Il.Mode.inputs mode)
  in
  (match
     Value_typecheck.check_values ~opaque_types:(Ctx.opaque_types ctx)
       ~find_typdef_opt:(fun id -> Ctx.find_typdef_opt ctx ~id)
       ~typs values
   with
  | Ok () -> ()
  | Error message ->
      Error.error no_region ("relation " ^ rid ^ " input: " ^ message));
  match Interp.invoke_rel ctx (rid $ no_region) values with
  | Some (ctx, values) -> (ctx, values)
  | None -> error no_region "relation was not matched"

let run_relation_fresh ~check_determinism ~opaque_types builtins cache spec rid
    values =
  Interp_runtime.Cache.clear cache;
  run_relation ~check_determinism ~opaque_types builtins cache spec rid values

type error = region * string

let run ?(check_determinism = false) (module T : Target.S) (spec : spec)
    (rid : string) (values : value list) : (Ctx.t * value list, error) result =
  let builtins = Builtins.make T.builtins in
  let cache =
    Interp_runtime.Cache.make ~is_impure_func:T.is_impure_func
      ~is_impure_rel:T.is_impure_rel ~state_version:T.state_version
  in
  try
    let opaque_types =
      match Opaque_types.make T.opaque_types with
      | Ok types -> types
      | Error message -> Error.error no_region message
    in
    run_relation_fresh ~check_determinism ~opaque_types builtins cache spec rid
      values
    |> Result.ok
  with InterpError (at, msg) | Interp_runtime.Context.Error (at, msg) ->
    Error (at, msg)

let error_to_diagnostic ((at, msg) : error) : Diag.t =
  Diag.error ~source:"sl-interp" at msg

module Ctx = Ctx
