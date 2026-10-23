open Common.Source
open Lang.Sl
open Error
module F = Format

let run_relation (ctx : Ctx.t) (spec : spec) (rid : id') (values : value list) :
    Ctx.t * value list =
  let ctx = Interp.load_spec ctx spec in
  let mode, _, _ = Ctx.find_rel Ctx.Global ctx (rid $ no_region) in
  let typs =
    List.map (fun exp -> exp.note $ exp.at) (Lang.Il.Mode.inputs mode)
  in
  (match
     Value_typecheck.check_values ~opaque_types:ctx.opaque_types
       ~find_typdef_opt:(Ctx.find_typdef_opt Ctx.Local ctx)
       ~typs values
   with
  | Ok () -> ()
  | Error message ->
      Error.error no_region ("relation " ^ rid ^ " input: " ^ message));
  match Interp.invoke_rel ctx (rid $ no_region) values with
  | Some (ctx, values) -> (ctx, values)
  | None -> error no_region "relation was not matched"

(* Entry point : Run typing rule *)

let run_relation_fresh ~(check_determinism : bool) ~opaque_types
    (filename : string) (builtins : Builtins.t) (cache : Cache.t) (spec : spec)
    (rid : id') (values : value list) : Ctx.t * value list =
  Cache.clear cache;
  let ctx =
    Ctx.empty ~check_determinism ~opaque_types filename builtins cache
  in
  run_relation ctx spec rid values

type error = region * string

let run ?(check_determinism = false) (module T : Target.S) (spec : spec)
    (rid : string) (values : value list) (filename : string) :
    (Ctx.t * value list, error) result =
  let builtins = Builtins.make T.builtins in
  let cache =
    Cache.make ~is_impure_func:T.is_impure_func ~is_impure_rel:T.is_impure_rel
      ~state_version:T.state_version
  in
  try
    let opaque_types =
      match Opaque_types.make T.opaque_types with
      | Ok types -> types
      | Error message -> Error.error no_region message
    in
    run_relation_fresh ~check_determinism ~opaque_types filename builtins cache
      spec rid values
    |> Result.ok
  with InterpError (at, msg) -> Error (at, msg)

let error_to_diagnostic ((at, msg) : error) : Diag.t =
  Diag.error ~source:"sl-interp" at msg

module Ctx = Ctx
