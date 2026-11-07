open Common.Annotated
module Location = Common.Location
module Il = Lang.Il
module Mixop = Lang.Il.Mixfix
module Value = Lang.Il.Value
open Il.Expression
open Lang.Sl
open Error
module Events = Instrumentation.Event
module F = Format

(* Option monad *)

let ( let* ) = Option.bind

module Expressions = Interp_runtime.Expression.Make (struct
  include Ctx

  type annotation = unit

  let get_exn = Interp_runtime.Context.get_exn
  let error = Error.error
end)

let rec eval_exp ctx exp = Expressions.eval_exp exp ~ctx ~call_func:invoke_func
and eval_exps ctx exps = Expressions.eval_exps exps ~ctx ~call_func:invoke_func
and eval_args ctx args = Expressions.eval_args args ~ctx ~call_func:invoke_func

(* Instruction evaluation *)

and eval_instr (ctx : Ctx.t) (instr : instr) : Sign.t =
  Instrumentation.Dispatcher.emit (Events.Instr { instr; at = instr.at });
  (* Result instruction evaluation *)
  let eval_result_instr ctx exps =
    let values = eval_exps ctx exps in
    Sign.Res values
  in
  (* Return instruction evaluation *)
  let eval_return_instr ctx exp =
    let value = eval_exp ctx exp in
    Sign.Ret value
  in
  (* Debug instruction evaluation *)
  let eval_debug_instr ctx exp =
    let value = eval_exp ctx exp in
    print_endline
    @@ F.sprintf "%s: %s"
         (Location.to_string exp.at)
         (Il.Print.string_of_exp exp);
    print_endline @@ Il.Print.string_of_value value
  in
  match instr.it with
  | RelI { call; iterinstrs; block } ->
      eval_rel_instr ctx call.relid call.notexp iterinstrs block
  | RelAssertI { call; expect; iterexps; block; phantom = _ } ->
      eval_rel_assert_instr ctx call.relid call.notexp expect iterexps block
  | IfI (exp_cond, iterexps, instrs_then, _phantom_opt) ->
      eval_if_instr ctx exp_cond iterexps instrs_then
  | CaseI (exp, cases, _phantom_opt) -> eval_case_instr ctx exp cases
  | OtherwiseI instr -> eval_instr ctx instr
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      eval_let_instr ctx exp_l exp_r iterinstrs block
  | ResultI exps -> eval_result_instr ctx exps
  | ReturnI exp -> eval_return_instr ctx exp
  | DebugI (exp, instr_body) ->
      eval_debug_instr ctx exp;
      eval_instr ctx instr_body

(* Each SL instruction contains one alternative's continuation. *)
and eval_instrs (ctx : Ctx.t) (instrs : instr list) : Sign.t =
  if Ctx.has_determinism_checks ctx then eval_instrs_unique ctx instrs
  else
    List.fold_left
      (fun sign instr ->
        match sign with Sign.Cont -> eval_instr ctx instr | _ -> sign)
      Sign.Cont instrs

and eval_instrs_unique (ctx : Ctx.t) (instrs : instr list) : Sign.t =
  let applied =
    List.fold_left
      (fun applied instr ->
        match (eval_instr ctx instr, applied) with
        | Sign.Cont, _ -> applied
        | sign, None -> Some (sign, instr.at)
        | _, Some (_, at_applied) ->
            error instr.at
              (F.asprintf
                 "this alternative applies, and so does the alternative at %s"
                 (Location.to_string at_applied)))
      None instrs
  in
  match applied with Some (sign, _) -> sign | None -> Sign.Cont

(* If instruction evaluation *)

and eval_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    (instrs_then : instr list) : Sign.t =
  let eval_cond ctx = eval_exp ctx exp_cond |> Value.get_bool in
  let cond =
    Ctx.for_all_body_contexts ctx ~iterexps ~f:eval_cond
    |> Interp_runtime.Context.get_exn
  in
  if cond then eval_instrs ctx instrs_then else Cont

(* Relation-assertion instruction evaluation *)

and eval_rel_assert_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
    (expect : bool) (iterexps : iterexp list) (block : instr list) : Sign.t =
  let eval_cond ctx =
    let values_input = eval_exps ctx (Mixop.args notexp) in
    match invoke_rel ctx id values_input with
    | Some _ -> expect
    | None -> not expect
  in
  let cond =
    Ctx.for_all_body_contexts ctx ~iterexps ~f:eval_cond
    |> Interp_runtime.Context.get_exn
  in
  if cond then eval_instrs ctx block else Cont

(* Case analysis instruction evaluation *)

and eval_case_instr (ctx : Ctx.t) (exp : exp) (cases : case list) : Sign.t =
  let eval_cases ctx exp cases =
    cases
    |> List.fold_left
         (fun (block_match, values_cond_rev) (guard, block) ->
           match block_match with
           | Some _ -> (block_match, values_cond_rev)
           | None ->
               let exp_cond =
                 match guard with
                 | BoolG true -> exp.it
                 | BoolG false -> UnE (`NotOp, `BoolT, exp)
                 | CmpG (cmpop, optyp, exp_r) -> CmpE (cmpop, optyp, exp, exp_r)
                 | SubG typ -> SubE (exp, typ)
                 | MatchG pattern -> MatchE (exp, pattern)
                 | MemG exp_s -> MemE (exp, exp_s)
               in
               let exp_cond =
                 exp_cond $$ (exp.at, { typ = Il.BoolT; annot = () })
               in
               let value_cond = eval_exp ctx exp_cond in
               let values_cond_rev = value_cond :: values_cond_rev in
               let cond = Value.get_bool value_cond in
               if cond then (Some block, values_cond_rev)
               else (None, values_cond_rev))
         (None, [])
    |> fun (block_match, values_cond) ->
    let value_cond =
      let typ_inner = Il.BoolT $ Location.Unknown in
      values_cond |> List.rev |> Value.list typ_inner
    in
    (block_match, value_cond)
  in
  let instrs_opt, _value_cond = eval_cases ctx exp cases in
  match instrs_opt with Some instrs -> eval_instrs ctx instrs | None -> Cont

(* Let instruction evaluation *)

and eval_let_instr (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (iterinstrs : iterinstr list) (block : block) : Sign.t =
  let eval_let ctx =
    let value = eval_exp ctx exp_r in
    Ok (Expressions.assign_exp ctx ~exp:exp_l ~value)
  in
  let ctx =
    Ctx.eval_iterations ctx ~iterinstrs ~f:eval_let
    |> Interp_runtime.Context.get_exn
  in
  eval_instrs ctx block

(* Relation instruction evaluation *)

and eval_rel_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
    (iterinstrs : iterinstr list) (block : block) : Sign.t =
  let eval_rel ctx =
    let mode, _, _ = Ctx.find_rel_exn ctx ~id in
    let exps_input, exps_output =
      Lang.Il.Mode.partition mode (Mixop.args notexp)
    in
    let values_input = eval_exps ctx exps_input in
    let values_output =
      match invoke_rel ctx id values_input with
      | Some values_output -> values_output
      | None -> error id.at "relation was not matched"
    in
    Ok (Expressions.assign_exps ctx ~exps:exps_output ~values:values_output)
  in
  let ctx =
    Ctx.eval_iterations ctx ~iterinstrs ~f:eval_rel
    |> Interp_runtime.Context.get_exn
  in
  eval_instrs ctx block

(* Invoke a relation *)

and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list) :
    value list option =
  Instrumentation.Dispatcher.emit
    (Events.Rel_enter { id = id.it; at = id.at; inputs = values_input });
  let mode, block, elseblock_opt = Ctx.find_rel_exn ctx ~id in
  let exps_input = Lang.Il.Mode.inputs mode in
  checkf
    (block <> [] || Option.is_some elseblock_opt)
    id.at "relation has no instructions";
  let attempt_rules () =
    Instrumentation.Dispatcher.emit
      (Events.Rule_enter { id = id.it; rule_id = "0"; at = id.at });
    let ctx_local = Ctx.for_call ctx in
    let ctx_local =
      Expressions.assign_exps ctx_local ~exps:exps_input ~values:values_input
    in
    let sign = eval_instrs ctx_local block in
    let sign =
      match (sign, elseblock_opt) with
      | Cont, Some elseblock -> eval_instrs ctx_local elseblock
      | _ -> sign
    in
    let result =
      match sign with Res values_output -> Some values_output | _ -> None
    in
    Instrumentation.Dispatcher.emit
      (Events.Rule_exit
         {
           id = id.it;
           rule_id = "0";
           at = id.at;
           success = Option.is_some result;
         });
    result
  in
  let result =
    let invoke () =
      match attempt_rules () with
      | Some values_output -> Ok values_output
      | None -> Error ()
    in
    match Ctx.memoize_rel ctx ~id ~values:values_input ~f:invoke with
    | Ok values_output, cached ->
        Interp_runtime.Emit_value.invoked ~callee:(Rel id.it)
          ~inputs:values_input ~outputs:values_output ~cached;
        Some values_output
    | Error _, _ -> None
  in
  Instrumentation.Dispatcher.emit_on_demand (fun () ->
      let outs =
        match result with
        | Some values_output -> List.map Option.some values_output
        | None -> List.map (fun _ -> None) (Lang.Il.Mode.outputs mode)
      in
      let conclusion = Lang.Il.Mode.fill mode ~ins:values_input ~outs in
      Events.Rel_exit
        { id = id.it; at = id.at; success = Option.is_some result; conclusion });
  result

(* Invoke a function *)

and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    value =
  let values_input = eval_args ctx args in
  let targs = Ctx.resolve_targs ctx ~targs in
  invoke_func_values ctx id targs values_input

and invoke_func_values (ctx : Ctx.t) (id : id) (targs : targ list)
    (values_input : value list) : value =
  (* Builtin function invocation *)
  let invoke_func_builtin signature =
    let value_output =
      Ctx.call_builtin ctx ~callback:(callback ctx) ~id ~targs
        ~values:values_input
      |> unwrap_builtin
    in
    (match
       Builtins.check_result ~opaque_types:(Ctx.opaque_types ctx)
         ~find_typdef_opt:(fun id -> Ctx.find_typdef_opt ctx ~id)
         ~signature ~targs value_output
     with
    | Ok () -> ()
    | Error message -> error id.at ("builtin $" ^ id.it ^ " result: " ^ message));
    value_output
  in
  (* User-defined function invocation *)
  let invoke_func_def tparams args_input block elseblock_opt =
    let ctx_local = Ctx.for_call ctx in
    checkf
      (List.length targs = List.length tparams)
      id.at "arity mismatch in type arguments";
    let ctx_local =
      List.fold_left2
        (fun ctx_local tparam targ ->
          Ctx.add_typdef_exn ctx_local ~id:tparam
            ~typdef:([], Il.PlainT targ $ targ.at))
        ctx_local tparams targs
    in
    let attempt_clauses () =
      Instrumentation.Dispatcher.emit
        (Events.Clause_enter { id = id.it; clause_idx = 0; at = id.at });
      let ctx_local =
        Expressions.assign_args ctx_local ~caller:ctx ~args:args_input
          ~values:values_input
      in
      let sign = eval_instrs ctx_local block in
      let sign =
        match (sign, elseblock_opt) with
        | Cont, Some elseblock -> eval_instrs ctx_local elseblock
        | _ -> sign
      in
      match sign with
      | Ret value_output ->
          Instrumentation.Dispatcher.emit
            (Events.Clause_exit
               { id = id.it; clause_idx = 0; at = id.at; success = true });
          value_output
      | _ ->
          Instrumentation.Dispatcher.emit
            (Events.Clause_exit
               { id = id.it; clause_idx = 0; at = id.at; success = false });
          error id.at "function was not matched"
    in
    attempt_clauses ()
  in
  (* Main dispatch *)
  Instrumentation.Dispatcher.emit
    (Events.Func_enter { id = id.it; at = id.at; inputs = values_input });
  let invoke_func' () =
    let invoke () =
      let v =
        match Ctx.find_func_opt ctx ~id with
        | Some (Ctx.Func.Builtin (tparams, _, typ)) ->
            checkf (Ctx.has_builtin ctx ~id) id.at
              "builtin $%s is declared in the spec but not implemented" id.it;
            invoke_func_builtin (tparams, typ)
        | Some (Ctx.Func.Defined (tparams, args_input, block, elseblock_opt)) ->
            checkf
              (block <> [] || Option.is_some elseblock_opt)
              id.at "function has no instructions";
            invoke_func_def tparams args_input block elseblock_opt
        | None ->
            if Ctx.has_builtin ctx ~id then
              error id.at
                (F.asprintf "builtin $%s is invoked without a spec declaration"
                   id.it)
            else error id.at (F.asprintf "unknown function %s" id.it)
      in
      Ok v
    in
    let has_function_arguments values =
      List.exists
        (fun value ->
          match value.it with Lang.Il.FuncV _ -> true | _ -> false)
        values
    in
    let value_output_result, cached =
      (* Cache keys omit type arguments and local function definitions. *)
      if
        targs <> [] || Ctx.has_local_func ctx ~id
        || has_function_arguments values_input
      then (invoke (), false)
      else Ctx.memoize_func ctx ~id ~values:values_input ~f:invoke
    in
    let value_output = Result.get_ok value_output_result in
    Interp_runtime.Emit_value.invoked ~callee:(Func id.it) ~inputs:values_input
      ~outputs:[ value_output ] ~cached;
    value_output
  in
  let result = invoke_func' () in
  Instrumentation.Dispatcher.emit
    (Events.Func_exit { id = id.it; at = id.at; output = Some result });
  result

and callback (ctx : Ctx.t) : Builtins.Callback.t =
 fun id targs values ->
  let ctx = Ctx.for_call ctx in
  let signature =
    match Ctx.find_func_opt ctx ~id with
    | Some func -> Ctx.Func.signature func
    | None -> error id.at ("unknown function " ^ id.it)
  in
  Ctx.check_inputs_exn ctx ~kind:`Function ~id ~signature ~targs ~values;
  invoke_func_values ctx id targs values

(* Load definitions into the context *)

let global_definition_of_def ~opaque_types (def : def) : Ctx.Global.definition =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      (match Opaque_types.check_declaration opaque_types ~id ~deftyp with
      | Ok () -> ()
      | Error message -> error id.at message);
      Ctx.Global.Syntax (id, (tparams, deftyp))
  | RelD (id, mode, block, elseblock_opt) ->
      Ctx.Global.Relation (id, (mode, block, elseblock_opt))
  | BuiltinDecD (id, tparams, args, typ) ->
      let typs = List.map Il.Expression.typ_of_arg args in
      Ctx.Global.Function (id, Ctx.Func.Builtin (tparams, typs, typ))
  | DecD (id, tparams, args_input, block, elseblock_opt) ->
      Ctx.Global.Function
        (id, Ctx.Func.Defined (tparams, args_input, block, elseblock_opt))

let context_of_spec (spec : spec) ~builtins ~cache ~opaque_types
    ~check_determinism : Ctx.t =
  let definitions = List.map (global_definition_of_def ~opaque_types) spec in
  let global = Ctx.Global.of_definitions_exn definitions in
  Ctx.make global ~builtins ~cache ~opaque_types ~check_determinism
