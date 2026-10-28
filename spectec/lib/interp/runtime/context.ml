open Common.Domain
open Common.Source
open Common.Attempt
module Il = Lang.Il
module Var = Envs.Interp.Var
module Value = Il.Value
module Typdef = Envs.Il.Typdef
module TDEnv = Envs.Il.TDEnv
module VEnv = Envs.Interp.VEnv
module IdHashtbl = Envs.Make.IdHashtbl
module FIdMap = Envs.Make.FIdMap

exception Error of region * string

let error_undef at kind id =
  raise (Error (at, Format.asprintf "%s `%s` is undefined" kind id))

let error_dup at kind id =
  raise (Error (at, Format.asprintf "%s `%s` was already defined" kind id))

let transpose_values value_matrix =
  match value_matrix with
  | [] -> Ok []
  | row :: rows ->
      let width = List.length row in
      let* () =
        guard
          (List.for_all (fun row -> List.length row = width) rows)
          no_region "cannot transpose a matrix of value batches"
      in
      let columns_rev =
        List.fold_left
          (fun columns_rev row ->
            List.map2 (fun column value -> value :: column) columns_rev row)
          (List.map (fun value -> [ value ]) row)
          rows
      in
      Ok (List.map List.rev columns_rev)

let get_exn = function
  | Ok value -> value
  | Result.Error failtraces ->
      let at = region_of_failtraces failtraces in
      let message =
        List.map (fun trace -> trace.message) failtraces |> String.concat "\n"
      in
      raise (Error (at, message))

module Make (Func : sig
  type t
end) (Rel : sig
  type t
end) =
struct
  module Global = struct
    type t = {
      syntaxes : Typdef.t IdHashtbl.t;
      functions : Func.t IdHashtbl.t;
      relations : Rel.t IdHashtbl.t;
    }

    type definition =
      | Syntax of TId.t * Typdef.t
      | Function of FId.t * Func.t
      | Relation of RId.t * Rel.t

    let of_definitions_exn definitions =
      let global =
        {
          syntaxes = IdHashtbl.create 16;
          functions = IdHashtbl.create 16;
          relations = IdHashtbl.create 16;
        }
      in
      let add table id value kind =
        if IdHashtbl.mem table id then error_dup id.at kind id.it;
        IdHashtbl.add table id value
      in
      List.iter
        (function
          | Syntax (id, typdef) -> add global.syntaxes id typdef "type"
          | Function (id, func) -> add global.functions id func "function"
          | Relation (id, rel) -> add global.relations id rel "relation")
        definitions;
      global
  end

  type local = {
    syntaxes : TDEnv.t;
    functions : Func.t FIdMap.t;
    values : VEnv.t;
  }

  type t = {
    global : Global.t;
    local : local;
    builtins : Builtins.t;
    opaque_types : Opaque_types.t;
    cache : Cache.t;
    check_determinism : bool;
  }

  let empty_local =
    { syntaxes = TDEnv.empty; functions = FIdMap.empty; values = VEnv.empty }

  let make global ~builtins ~cache ~opaque_types ~check_determinism =
    {
      global;
      local = empty_local;
      builtins;
      cache;
      opaque_types;
      check_determinism;
    }

  let find_value_opt ctx ~var = VEnv.find_opt var ctx.local.values

  let find_value_exn ctx ~var =
    match find_value_opt ctx ~var with
    | Some value -> value
    | None ->
        let id, _ = var in
        error_undef id.at "value" (Var.to_string var)

  let find_typdef_opt ctx ~id =
    match TDEnv.find_opt id ctx.local.syntaxes with
    | Some typdef -> Some typdef
    | None -> IdHashtbl.find_opt ctx.global.syntaxes id

  let find_typdef_exn ctx ~id =
    match find_typdef_opt ctx ~id with
    | Some typdef -> typdef
    | None -> error_undef id.at "type" id.it

  let find_func_opt ctx ~id =
    match FIdMap.find_opt id ctx.local.functions with
    | Some func -> Some func
    | None -> IdHashtbl.find_opt ctx.global.functions id

  let find_func_exn ctx ~id =
    match find_func_opt ctx ~id with
    | Some func -> func
    | None -> error_undef id.at "function" id.it

  let find_rel_opt ctx ~id = IdHashtbl.find_opt ctx.global.relations id

  let find_rel_exn ctx ~id =
    match find_rel_opt ctx ~id with
    | Some rel -> rel
    | None -> error_undef id.at "relation" id.it

  let has_value ctx ~var = VEnv.mem var ctx.local.values
  let has_local_func ctx ~id = FIdMap.mem id ctx.local.functions

  let replace_values ctx values =
    if values == ctx.local.values then ctx
    else { ctx with local = { ctx.local with values } }

  let set_value ctx ~var ~value =
    replace_values ctx (VEnv.add var value ctx.local.values)

  let set_values ctx ~bindings =
    let values =
      List.fold_left
        (fun values (var, value) -> VEnv.add var value values)
        ctx.local.values bindings
    in
    replace_values ctx values

  let add_value_exn ctx ~var ~value =
    let id, _ = var in
    if has_value ctx ~var then error_dup id.at "value" (Var.to_string var);
    set_value ctx ~var ~value

  let add_values_exn ctx ~bindings =
    let values =
      List.fold_left
        (fun values (var, value) ->
          let id, _ = var in
          if VEnv.mem var values then
            error_dup id.at "value" (Var.to_string var);
          VEnv.add var value values)
        ctx.local.values bindings
    in
    replace_values ctx values

  let add_typdef_exn ctx ~id ~typdef =
    if Option.is_some (find_typdef_opt ctx ~id) then
      error_dup id.at "type" id.it;
    let syntaxes = TDEnv.add id typdef ctx.local.syntaxes in
    { ctx with local = { ctx.local with syntaxes } }

  let add_typdefs_exn ctx ~bindings =
    let syntaxes =
      List.fold_left
        (fun syntaxes (id, typdef) ->
          if TDEnv.mem id syntaxes || IdHashtbl.mem ctx.global.syntaxes id then
            error_dup id.at "type" id.it;
          TDEnv.add id typdef syntaxes)
        ctx.local.syntaxes bindings
    in
    { ctx with local = { ctx.local with syntaxes } }

  let add_func_exn ctx ~id ~func =
    if Option.is_some (find_func_opt ctx ~id) then
      error_dup id.at "function" id.it;
    let functions = FIdMap.add id func ctx.local.functions in
    { ctx with local = { ctx.local with functions } }

  let for_call ctx = { ctx with local = empty_local }
  let without_values ctx = replace_values ctx VEnv.empty

  let fold_local_typdefs ctx ~init ~f =
    TDEnv.fold (fun id typdef acc -> f acc id typdef) ctx.local.syntaxes init

  let for_optional_iteration ctx ~vars =
    let values =
      List.map
        (fun { Il.varid; iters; _ } ->
          find_value_exn ctx ~var:(varid, iters @ [ Il.Opt ]) |> Value.get_opt)
        vars
    in
    if List.for_all Option.is_some values then
      let bindings =
        List.map2
          (fun { Il.varid; iters; _ } value ->
            ((varid, iters), Option.get value))
          vars values
      in
      Ok (Some (set_values ctx ~bindings))
    else if List.for_all Option.is_none values then Ok None
    else fail no_region "mismatch in optionality of iterated variables"

  let for_list_iteration ctx ~vars =
    let* values_batch =
      List.map
        (fun { Il.varid; iters; _ } ->
          find_value_exn ctx ~var:(varid, iters @ [ Il.List ]) |> Value.get_list)
        vars
      |> transpose_values
    in
    let ctxs =
      List.map
        (fun values ->
          let bindings =
            List.map2
              (fun { Il.varid; iters; _ } value -> ((varid, iters), value))
              vars values
          in
          set_values ctx ~bindings)
        values_batch
    in
    Ok ctxs

  let expand_iteration_inputs ctx ~iter ~vars =
    match iter with
    | Il.Opt ->
        let* ctx_opt = for_optional_iteration ctx ~vars in
        Ok (Option.to_list ctx_opt)
    | Il.List -> for_list_iteration ctx ~vars

  let collect_iteration_value values ~iter ~result_typ =
    match (iter, values) with
    | Il.Opt, [] -> Value.Make.opt result_typ None
    | Il.Opt, [ value ] -> Value.Make.opt result_typ (Some value)
    | Il.Opt, _ ->
        raise
          (Error (no_region, "an option iteration has more than one element"))
    | Il.List, _ -> Value.Make.list result_typ values

  let collect_iteration_bindings ctxs ~iter ~vars =
    List.map
      (fun { Il.varid; typ; iters } ->
        let values =
          List.map (fun ctx -> find_value_exn ctx ~var:(varid, iters)) ctxs
        in
        let iters = iters @ [ iter ] in
        let typ = (Il.Typ.iterate typ iters).it in
        ((varid, iters), collect_iteration_value values ~iter ~result_typ:typ))
      vars

  let for_all_body_contexts ctx ~iterexps ~f =
    let iterate body (iter, vars) ctx =
      let* ctxs = expand_iteration_inputs ctx ~iter ~vars in
      List.fold_left
        (fun holds ctx -> match holds with Ok true -> body ctx | _ -> holds)
        (Ok true) ctxs
    in
    List.fold_left iterate (fun ctx -> Ok (f ctx)) iterexps ctx

  let has_determinism_checks ctx = ctx.check_determinism
  let opaque_types ctx = ctx.opaque_types
  let has_builtin ctx ~id = ctx.builtins.is_builtin id
  let call_builtin ctx ~id ~targs ~values = ctx.builtins.invoke id targs values

  let memoize_func ctx ~id ~values ~f =
    Cache.with_func_cache ctx.cache (id.it, values) f

  let memoize_rel ctx ~id ~values ~f =
    Cache.with_rel_cache ctx.cache (id.it, values) f
end
