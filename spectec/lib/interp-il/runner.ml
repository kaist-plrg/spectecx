open Il
module Cache = Runtime_dynamic.Cache
module F = Format
open Attempt
open Util.Source

let run_relation (ctx : Ctx.t) (spec : spec) (rid : id') (values : value list) :
    Ctx.t * value list =
  let ctx = Interp.load_spec ctx spec in
  let+ ctx, values = Interp.invoke_rel ctx (rid $ no_region) values in
  (ctx, values)

let init ?(debug : bool = false) ?(profile : bool = false)
    (filename_target : string) : Ctx.t =
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  Ctx.empty ~debug ~profile filename_target
