open Sl.Ast
module Cache = Runtime_dynamic.Cache
open Error
module F = Format
open Util.Source

let run_relation (ctx : Ctx.t) (spec : spec) (rid : id') (values : value list) :
    Ctx.t * value list =
  let ctx = Interp.load_spec ctx spec in
  match Interp.invoke_rel ctx (rid $ no_region) values with
  | Some (ctx, values) -> (ctx, values)
  | None -> error no_region "relation was not matched"

(* Entry point : Run typing rule *)

let init (filename_target : string) : Ctx.t =
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  Ctx.empty filename_target

let run_relation_fresh (spec : spec) (rid : id') (values : value list)
    (filename_target : string) : Ctx.t * value list =
  let ctx = init filename_target in
  run_relation ctx spec rid values
