open Common.Source
module Il = Lang.Il
module Mixop = Lang.Il.Mixfix
module Value = Lang.Il.Value
open Il.Expression
open Lang.Sl
open Envs.Make
module Typ = Envs.Il.Typ
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
    @@ F.sprintf "%s: %s" (string_of_region exp.at) (Il.Print.string_of_exp exp);
    print_endline @@ Il.Print.string_of_value value
  in
  match instr.it with
  | RelI { call; iterexps; block } ->
      eval_rel_instr ctx call.relid call.notexp iterexps block
  | RelAssertI { call; expect; iterexps; block; phantom = _ } ->
      eval_rel_assert_instr ctx call.relid call.notexp expect iterexps block
  | IfI (exp_cond, iterexps, instrs_then, _phantom_opt) ->
      eval_if_instr ctx exp_cond iterexps instrs_then
  | CaseI (exp, cases, _phantom_opt) -> eval_case_instr ctx exp cases
  | OtherwiseI instr -> eval_instr ctx instr
  | LetI (exp_l, exp_r, iterexps, block) ->
      eval_let_instr ctx exp_l exp_r iterexps block
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
                 (string_of_region at_applied)))
      None instrs
  in
  match applied with Some (sign, _) -> sign | None -> Sign.Cont

(* If instruction evaluation *)

and eval_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    (instrs_then : instr list) : Sign.t =
  let eval_if_cond_iter ctx exp_cond iterexps =
    let rec eval_if_cond_iter' ctx exp_cond iterexps =
      let eval_if_cond ctx exp_cond =
        let value_cond = eval_exp ctx exp_cond in
        let cond = Value.get_bool value_cond in
        (cond, value_cond)
      in
      let eval_if_cond_opt ctx exp_cond vars iterexps =
        match
          Interp_runtime.Context.get_exn (Ctx.for_optional_iteration ctx ~vars)
        with
        | Some ctx_sub ->
            let cond, value_cond =
              eval_if_cond_iter' ctx_sub exp_cond iterexps
            in
            (cond, Some value_cond)
        | None -> (true, None)
      in
      let eval_if_cond_list ctx exp_cond vars iterexps =
        let ctxs_sub =
          Interp_runtime.Context.get_exn (Ctx.for_list_iteration ctx ~vars)
        in
        let cond, values_cond_rev =
          List.fold_left
            (fun (cond, values_cond_rev) ctx_sub ->
              if not cond then (cond, values_cond_rev)
              else
                let cond, value_cond =
                  eval_if_cond_iter' ctx_sub exp_cond iterexps
                in
                let values_cond_rev = value_cond :: values_cond_rev in
                (cond, values_cond_rev))
            (true, []) ctxs_sub
        in
        (cond, List.rev values_cond_rev)
      in
      match iterexps with
      | [] -> eval_if_cond ctx exp_cond
      | iterexp_h :: iterexps_t -> (
          let iter_h, vars_h = iterexp_h in
          match iter_h with
          | Il.Opt ->
              let cond, value_cond_opt =
                eval_if_cond_opt ctx exp_cond vars_h iterexps_t
              in
              let typ_inner = Il.BoolT $ no_region in
              let value_cond = Value.opt typ_inner value_cond_opt in
              (cond, value_cond)
          | Il.List ->
              let cond, values_cond =
                eval_if_cond_list ctx exp_cond vars_h iterexps_t
              in
              let value_cond =
                let typ_inner = Il.BoolT $ no_region in
                Value.list typ_inner values_cond
              in
              (cond, value_cond))
    in
    let iterexps = List.rev iterexps in
    eval_if_cond_iter' ctx exp_cond iterexps
  in
  let cond, _value_cond = eval_if_cond_iter ctx exp_cond iterexps in
  if cond then eval_instrs ctx instrs_then else Cont

(* Relation-assertion instruction evaluation *)

and eval_rel_assert_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
    (expect : bool) (iterexps : iterexp list) (block : instr list) : Sign.t =
  let eval_rel_assert_cond_iter ctx id notexp iterexps =
    let rec eval_rel_assert_cond_iter' ctx id notexp iterexps =
      let eval_rel_assert_cond ctx id notexp =
        let values_input = eval_exps ctx (Mixop.args notexp) in
        let cond =
          match invoke_rel ctx id values_input with
          | Some _ -> expect
          | None -> not expect
        in
        let value_res = Value.Make.bool Il.BoolT cond in
        (cond, value_res)
      in
      let eval_rel_assert_cond_opt ctx id notexp vars iterexps =
        match
          Interp_runtime.Context.get_exn (Ctx.for_optional_iteration ctx ~vars)
        with
        | Some ctx_sub ->
            let cond, value_cond =
              eval_rel_assert_cond_iter' ctx_sub id notexp iterexps
            in
            (cond, Some value_cond)
        | None -> (true, None)
      in
      let eval_rel_assert_cond_list ctx id notexp vars iterexps =
        let ctxs_sub =
          Interp_runtime.Context.get_exn (Ctx.for_list_iteration ctx ~vars)
        in
        let cond, values_cond_rev =
          List.fold_left
            (fun (cond, values_cond_rev) ctx_sub ->
              if not cond then (cond, values_cond_rev)
              else
                let cond, value_cond =
                  eval_rel_assert_cond_iter' ctx_sub id notexp iterexps
                in
                let values_cond_rev = value_cond :: values_cond_rev in
                (cond, values_cond_rev))
            (true, []) ctxs_sub
        in
        (cond, List.rev values_cond_rev)
      in
      match iterexps with
      | [] -> eval_rel_assert_cond ctx id notexp
      | iterexp_h :: iterexps_t -> (
          let iter_h, vars_h = iterexp_h in
          match iter_h with
          | Il.Opt ->
              let cond, value_cond_opt =
                eval_rel_assert_cond_opt ctx id notexp vars_h iterexps_t
              in
              let typ_inner = Il.BoolT $ no_region in
              let value_cond = Value.opt typ_inner value_cond_opt in
              (cond, value_cond)
          | Il.List ->
              let cond, values_cond =
                eval_rel_assert_cond_list ctx id notexp vars_h iterexps_t
              in
              let value_cond =
                let typ_inner = Il.BoolT $ no_region in
                Value.list typ_inner values_cond
              in
              (cond, value_cond))
    in
    let iterexps = List.rev iterexps in
    eval_rel_assert_cond_iter' ctx id notexp iterexps
  in
  let cond, _value_cond = eval_rel_assert_cond_iter ctx id notexp iterexps in
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
      let typ_inner = Il.BoolT $ no_region in
      values_cond |> List.rev |> Value.list typ_inner
    in
    (block_match, value_cond)
  in
  let instrs_opt, _value_cond = eval_cases ctx exp cases in
  match instrs_opt with Some instrs -> eval_instrs ctx instrs | None -> Cont

(* Let instruction evaluation *)

and eval_let_instr (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (iterexps : iterexp list) (block : block) : Sign.t =
  let eval_let_iter ctx exp_l exp_r iterexps =
    let rec eval_let_iter' ctx exp_l exp_r iterexps =
      let eval_let ctx exp_l exp_r =
        let value = eval_exp ctx exp_r in
        Expressions.assign_exp ctx ~exp:exp_l ~value
      in
      let eval_let_opt ctx exp_l exp_r vars iterexps =
        (* Discriminate between bound and binding variables *)
        let vars_bound, vars_binding =
          List.partition
            (fun { Il.varid = id; iters; _ } ->
              Ctx.has_value ctx ~var:(id, iters @ [ Il.Opt ]))
            vars
        in
        let ctx_sub_opt =
          Interp_runtime.Context.get_exn
            (Ctx.for_optional_iteration ctx ~vars:vars_bound)
        in
        let ctx, values_binding =
          match ctx_sub_opt with
          (* If the bound variable supposed to guide the iteration is already empty,
            then the binding variables are also empty *)
          | None ->
              let values_binding =
                List.map
                  (fun { Il.typ = typ_binding; iters = iters_binding; _ } ->
                    let value_binding =
                      let typ =
                        Il.Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                      in
                      None |> Value.Make.opt typ.it
                    in
                    value_binding)
                  vars_binding
              in
              (ctx, values_binding)
          (* Otherwise, evaluate the premise for the subcontext *)
          | Some ctx_sub ->
              let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
              let values_binding =
                List.map
                  (fun {
                         Il.varid = id_binding;
                         typ = typ_binding;
                         iters = iters_binding;
                       } ->
                    let value_binding =
                      Ctx.find_value_exn ctx_sub ~var:(id_binding, iters_binding)
                    in
                    let value_binding =
                      let typ =
                        Il.Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                      in
                      Some value_binding |> Value.Make.opt typ.it
                    in
                    value_binding)
                  vars_binding
              in
              (ctx, values_binding)
        in
        (* Finally, bind the resulting values *)
        List.fold_left2
          (fun ctx { Il.varid = id_binding; iters = iters_binding; _ }
               value_binding ->
            Ctx.set_value ctx
              ~var:(id_binding, iters_binding @ [ Il.Opt ])
              ~value:value_binding)
          ctx vars_binding values_binding
      in
      let eval_let_list ctx exp_l exp_r vars iterexps =
        (* Discriminate between bound and binding variables *)
        let vars_bound, vars_binding =
          List.partition
            (fun { Il.varid = id; iters; _ } ->
              Ctx.has_value ctx ~var:(id, iters @ [ Il.List ]))
            vars
        in
        (* Create a subcontext for each batch of bound values *)
        let ctxs_sub =
          Interp_runtime.Context.get_exn
            (Ctx.for_list_iteration ctx ~vars:vars_bound)
        in
        let ctx, values_binding =
          match ctxs_sub with
          (* If the bound variable supposed to guide the iteration is already empty,
            then the binding variables are also empty *)
          | [] ->
              let values_binding =
                List.init (List.length vars_binding) (fun _ -> [])
              in
              (ctx, values_binding)
              (* Otherwise, evaluate the premise for each batch of bound values,
            and collect the resulting binding batches *)
          | _ ->
              let values_binding_batch =
                List.map
                  (fun ctx_sub ->
                    let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
                    List.map
                      (fun { Il.varid = id_binding; iters = iters_binding; _ }
                         ->
                        Ctx.find_value_exn ctx_sub
                          ~var:(id_binding, iters_binding))
                      vars_binding)
                  ctxs_sub
              in
              let values_binding =
                Interp_runtime.Context.transpose_values values_binding_batch
                |> Interp_runtime.Context.get_exn
              in
              (ctx, values_binding)
        in
        (* Finally, bind the resulting binding batches *)
        List.fold_left2
          (fun ctx
               {
                 Il.varid = id_binding;
                 typ = typ_binding;
                 iters = iters_binding;
               } values_binding ->
            let value_binding =
              let typ =
                Il.Typ.iterate typ_binding (iters_binding @ [ Il.List ])
              in
              values_binding |> Value.Make.list typ.it
            in
            Ctx.set_value ctx
              ~var:(id_binding, iters_binding @ [ Il.List ])
              ~value:value_binding)
          ctx vars_binding values_binding
      in
      match iterexps with
      | [] -> eval_let ctx exp_l exp_r
      | iterexp_h :: iterexps_t -> (
          let iter_h, vars_h = iterexp_h in
          match iter_h with
          | Il.Opt -> eval_let_opt ctx exp_l exp_r vars_h iterexps_t
          | Il.List -> eval_let_list ctx exp_l exp_r vars_h iterexps_t)
    in
    let iterexps = List.rev iterexps in
    eval_let_iter' ctx exp_l exp_r iterexps
  in
  let ctx = eval_let_iter ctx exp_l exp_r iterexps in
  eval_instrs ctx block

(* Relation instruction evaluation *)

and eval_rel_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
    (iterexps : iterexp list) (block : block) : Sign.t =
  let eval_rel_iter ctx id notexp iterexps =
    let rec eval_rel_iter' ctx id notexp iterexps =
      (* Single relation evaluation *)
      let eval_rel ctx id notexp =
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
        Expressions.assign_exps ctx ~exps:exps_output ~values:values_output
      in
      (* Relation list evaluation *)
      let eval_rel_list ctx id notexp vars iterexps =
        (* Discriminate between bound and binding variables *)
        let vars_bound, vars_binding =
          List.partition
            (fun { Il.varid = id; iters; _ } ->
              Ctx.has_value ctx ~var:(id, iters @ [ Il.List ]))
            vars
        in
        (* Create a subcontext for each batch of bound values *)
        let ctxs_sub =
          Interp_runtime.Context.get_exn
            (Ctx.for_list_iteration ctx ~vars:vars_bound)
        in
        let ctx, values_binding =
          match ctxs_sub with
          (* If the bound variable supposed to guide the iteration is already empty,
            then the binding variables are also empty *)
          | [] ->
              let values_binding =
                List.init (List.length vars_binding) (fun _ -> [])
              in
              (ctx, values_binding)
          (* Otherwise, evaluate the premise for each batch of bound values,
            and collect the resulting binding batches *)
          | _ ->
              let values_binding_batch =
                List.map
                  (fun ctx_sub ->
                    let ctx_sub = eval_rel_iter' ctx_sub id notexp iterexps in
                    List.map
                      (fun { Il.varid = id_binding; iters = iters_binding; _ }
                         ->
                        Ctx.find_value_exn ctx_sub
                          ~var:(id_binding, iters_binding))
                      vars_binding)
                  ctxs_sub
              in
              let values_binding =
                Interp_runtime.Context.transpose_values values_binding_batch
                |> Interp_runtime.Context.get_exn
              in
              (ctx, values_binding)
        in
        (* Finally, bind the resulting binding batches *)
        List.fold_left2
          (fun ctx
               {
                 Il.varid = id_binding;
                 typ = typ_binding;
                 iters = iters_binding;
               } values_binding ->
            let value_binding =
              let typ =
                Il.Typ.iterate typ_binding (iters_binding @ [ Il.List ])
              in
              values_binding |> Value.Make.list typ.it
            in
            Ctx.set_value ctx
              ~var:(id_binding, iters_binding @ [ Il.List ])
              ~value:value_binding)
          ctx vars_binding values_binding
      in
      (* Optional relation evaluation *)
      let eval_rel_opt ctx id notexp vars iterexps =
        let vars_bound, vars_binding =
          List.partition
            (fun { Il.varid = id; iters; _ } ->
              Ctx.has_value ctx ~var:(id, iters @ [ Il.Opt ]))
            vars
        in
        let ctx_sub_opt =
          Interp_runtime.Context.get_exn
            (Ctx.for_optional_iteration ctx ~vars:vars_bound)
        in
        let ctx, values_binding =
          match ctx_sub_opt with
          | None ->
              let values_binding =
                List.map
                  (fun { Il.typ = typ_binding; iters = iters_binding; _ } ->
                    let typ =
                      Il.Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                    in
                    None |> Value.Make.opt typ.it)
                  vars_binding
              in
              (ctx, values_binding)
          | Some ctx_sub ->
              let ctx_sub = eval_rel_iter' ctx_sub id notexp iterexps in
              let values_binding =
                List.map
                  (fun {
                         Il.varid = id_binding;
                         typ = typ_binding;
                         iters = iters_binding;
                       } ->
                    let value_binding =
                      Ctx.find_value_exn ctx_sub ~var:(id_binding, iters_binding)
                    in
                    let typ =
                      Il.Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                    in
                    Some value_binding |> Value.Make.opt typ.it)
                  vars_binding
              in
              (ctx, values_binding)
        in
        List.fold_left2
          (fun ctx { Il.varid = id_binding; iters = iters_binding; _ }
               value_binding ->
            Ctx.set_value ctx
              ~var:(id_binding, iters_binding @ [ Il.Opt ])
              ~value:value_binding)
          ctx vars_binding values_binding
      in
      match iterexps with
      | [] -> eval_rel ctx id notexp
      | iterexp_h :: iterexps_t -> (
          let iter_h, vars_h = iterexp_h in
          match iter_h with
          | Il.Opt -> eval_rel_opt ctx id notexp vars_h iterexps_t
          | Il.List -> eval_rel_list ctx id notexp vars_h iterexps_t)
    in
    let iterexps = List.rev iterexps in
    eval_rel_iter' ctx id notexp iterexps
  in
  let ctx = eval_rel_iter ctx id notexp iterexps in
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
    | Ok values_output -> Some values_output
    | Error _ -> None
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
  (* Builtin function invocation *)
  let invoke_func_builtin signature =
    let value_output =
      Ctx.call_builtin ctx ~id ~targs ~values:values_input |> unwrap_builtin
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
    let targs =
      match targs with
      | [] -> []
      | targs ->
          let theta =
            Ctx.fold_local_typdefs ctx ~init:TIdMap.empty
              ~f:(fun theta tid typdef ->
                let tparams, deftyp = typdef in
                match (tparams, deftyp.it) with
                | [], Il.PlainT typ -> TIdMap.add tid typ theta
                | _ -> theta)
          in
          List.map (Typ.subst_typ theta) targs
    in
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
        | Some (Ctx.Func.Builtin (tparams, typ)) ->
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
    let value_output_result =
      (* Cache keys omit type arguments and local function definitions. *)
      if
        targs <> [] || Ctx.has_local_func ctx ~id
        || has_function_arguments values_input
      then invoke ()
      else Ctx.memoize_func ctx ~id ~values:values_input ~f:invoke
    in
    value_output_result |> Result.get_ok
  in
  let result = invoke_func' () in
  Instrumentation.Dispatcher.emit
    (Events.Func_exit { id = id.it; at = id.at; output = Some result });
  result

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
  | BuiltinDecD (id, tparams, _, typ) ->
      Ctx.Global.Function (id, Ctx.Func.Builtin (tparams, typ))
  | DecD (id, tparams, args_input, block, elseblock_opt) ->
      Ctx.Global.Function
        (id, Ctx.Func.Defined (tparams, args_input, block, elseblock_opt))

let context_of_spec (spec : spec) ~builtins ~cache ~opaque_types
    ~check_determinism : Ctx.t =
  let definitions = List.map (global_definition_of_def ~opaque_types) spec in
  let global = Ctx.Global.of_definitions_exn definitions in
  Ctx.make global ~builtins ~cache ~opaque_types ~check_determinism
