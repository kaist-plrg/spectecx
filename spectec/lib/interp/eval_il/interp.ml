open Common.Source
open Lang.Il.Expression
open Lang.Il
open Envs.Make
module Typ = Envs.Il.Typ
open Error
open Attempt
module Events = Instrumentation.Event
module F = Format

(* Structuring also identifies fallback paths by a trailing ElsePr. *)

let is_fallback_prems (prems : prem list) : bool =
  match List.rev prems with { it = ElsePr; _ } :: _ -> true | _ -> false

let is_fallback_rule (rule : rule) : bool =
  let ({ prems; _ } : rule') = rule.it in
  is_fallback_prems prems

let is_fallback_clause (clause : clause) : bool =
  let ({ prems; _ } : clause') = clause.it in
  is_fallback_prems prems

let describe_rule (id_rel : id) (rule : rule) : string =
  let ({ ruleid = id_rule; _ } : rule') = rule.it in
  if id_rule.it = "" then F.asprintf "the rule at %s" (string_of_region rule.at)
  else F.asprintf "rule %s/%s" id_rel.it id_rule.it

(* Assignments *)

(* Assigning a value to an expression *)

let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
  let note = value.note.typ in
  match (exp.it, value.it) with
  | VarE id, _ ->
      let ctx = Ctx.add_value_exn ctx ~var:(id, []) ~value in
      ctx
  | TupleE exps, TupleV values -> assign_exps ctx exps values
  | CaseE notexp, CaseV valuecase ->
      let exps = Mixfix.args notexp in
      let values = Mixfix.args valuecase in
      assign_exps ctx exps values
  | StrE expfields, StructV valuefields ->
      let exps = List.map snd expfields in
      let values = List.map snd valuefields in
      assign_exps ctx exps values
  | OptE exp_opt, OptV value_opt -> (
      match (exp_opt, value_opt) with
      | Some exp, Some value -> assign_exp ctx exp value
      | None, None -> ctx
      | _ -> assert false)
  | ListE exps, ListV values -> assign_exps ctx exps values
  | ConsE (exp_h, exp_t), ListV values_inner ->
      let value_h = List.hd values_inner in
      let value_t = List.tl values_inner |> Value.Make.list note in
      let ctx = assign_exp ctx exp_h value_h in
      assign_exp ctx exp_t value_t
  | IterE (_, (Opt, vars)), OptV None ->
      (* Per iterated variable, make an option out of the value *)
      let bindings =
        List.map
          (fun { varid = id; typ; iters } ->
            let value_sub =
              let typ = Lang.Il.Typ.iterate typ (iters @ [ Opt ]) in
              None |> Value.Make.opt typ.it
            in
            ((id, iters @ [ Opt ]), value_sub))
          vars
      in
      Ctx.add_values_exn ctx ~bindings
  | IterE (exp, (Opt, vars)), OptV (Some value) ->
      (* Assign the value to the iterated expression *)
      let ctx_inner = assign_exp ctx exp value in
      (* Per iterated variable, make an option out of the value *)
      let bindings =
        List.map
          (fun { varid = id; typ; iters } ->
            let value_sub =
              let value = Ctx.find_value_exn ctx_inner ~var:(id, iters) in
              let typ = Lang.Il.Typ.iterate typ (iters @ [ Opt ]) in
              Some value |> Value.Make.opt typ.it
            in
            ((id, iters @ [ Opt ]), value_sub))
          vars
      in
      Ctx.add_values_exn ctx ~bindings
  | IterE (exp, (List, vars)), ListV values ->
      (* Map over the value list elements,
         and assign each value to the iterated expression *)
      let ctxs_rev =
        List.fold_left
          (fun ctxs_rev value ->
            let ctx = Ctx.without_values ctx in
            let ctx = assign_exp ctx exp value in
            ctx :: ctxs_rev)
          [] values
      in
      let ctxs = List.rev ctxs_rev in
      (* Per iterated variable, collect its elementwise value,
         then make a sequence out of them *)
      List.fold_left
        (fun ctx { varid = id; typ; iters } ->
          let values =
            List.map (fun ctx -> Ctx.find_value_exn ctx ~var:(id, iters)) ctxs
          in
          let value_sub =
            let typ = Lang.Il.Typ.iterate typ (iters @ [ List ]) in
            values |> Value.Make.list typ.it
          in
          Ctx.add_value_exn ctx ~var:(id, iters @ [ List ]) ~value:value_sub)
        ctx vars
  | _ ->
      error exp.at
        (F.asprintf "(TODO) match failed %s <- %s" (Print.string_of_exp exp)
           (Print.string_of_value ~short:true value))

and assign_exps (ctx : Ctx.t) (exps : exp list) (values : value list) : Ctx.t =
  let region = over_region (List.map at exps) in
  checkf
    (List.length exps = List.length values)
    region
    "mismatch in number of expressions and values while assigning, expected %d \
     value(s) but got %d"
    (List.length exps) (List.length values);
  List.fold_left2 assign_exp ctx exps values

(* Assigning a value to an argument *)

and assign_arg (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (arg : arg)
    (value : value) : Ctx.t =
  let assign_arg_exp ctx exp value = assign_exp ctx exp value in
  let assign_arg_def ctx_caller ctx_callee id value =
    match value.it with
    | FuncV id_f ->
        let func = Ctx.find_func_exn ctx_caller ~id:id_f in
        Ctx.add_func_exn ctx_callee ~id ~func
    | _ ->
        error id.at
          (F.asprintf "cannot assign a value %s to a definition %s"
             (Print.string_of_value ~short:true value)
             id.it)
  in
  match arg.it with
  | ExpA exp -> assign_arg_exp ctx_callee exp value
  | DefA id -> assign_arg_def ctx_caller ctx_callee id value

and assign_args (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (args : arg list)
    (values : value list) : Ctx.t =
  let region = over_region (List.map at args) in
  checkf
    (List.length args = List.length values)
    region
    "mismatch in number of arguments and values while assigning, expected %d \
     value(s) but got %d"
    (List.length args) (List.length values);
  List.fold_left2 (assign_arg ctx_caller) ctx_callee args values

module Expressions = Interp_runtime.Expression.Make (struct
  include Ctx

  type annotation = unit

  let get_exn = Attempt.unwrap_or_error
  let error = Error.error
end)

let rec eval_exp ctx exp =
  Expressions.eval_exp exp ~ctx ~call_func:call_func_or_error

and eval_exps ctx exps =
  Expressions.eval_exps exps ~ctx ~call_func:call_func_or_error

and eval_args ctx args =
  Expressions.eval_args args ~ctx ~call_func:call_func_or_error

and call_func_or_error ctx id targs args =
  invoke_func ctx id targs args |> Attempt.unwrap_or_error

(* Premise evaluation *)

and eval_prem (ctx : Ctx.t) (prem : prem) : Ctx.t attempt =
  let eval_rule_prem ctx id notexp =
    let reltyp, _ = Ctx.find_rel_exn ctx ~id in
    let exps_input, exps_output =
      Mode.partition reltyp.it (Mixfix.args notexp)
    in
    let values_input = eval_exps ctx exps_input in
    let* values_output = invoke_rel ctx id values_input in
    let ctx = assign_exps ctx exps_output values_output in
    Ok ctx
  in
  let eval_cond_prem fail_with ctx exp_cond =
    let value_cond = eval_exp ctx exp_cond in
    let cond = Value.get_bool value_cond in
    if cond then Ok ctx
    else
      fail_with exp_cond.at
        (F.asprintf "condition %s was not met" (Print.string_of_exp exp_cond))
  in
  let eval_rel_assert_prem ctx (call : relcall) (expect : bool) =
    let id = call.relid in
    let values_input = eval_exps ctx (Mixfix.args call.notexp) in
    match (invoke_rel ctx id values_input, expect) with
    | Ok _, true | Error _, false -> Ok ctx
    | Ok _, false ->
        fail id.at (F.asprintf "condition not-hold %s was not met" id.it)
    | Error failtraces, true -> Error failtraces
  in
  let eval_let_prem ctx exp_l exp_r =
    let value = eval_exp ctx exp_r in
    let ctx = assign_exp ctx exp_l value in
    Ok ctx
  in
  let eval_debug_prem ctx exp =
    let value = eval_exp ctx exp in
    print_endline
    @@ F.sprintf "%s: %s" (string_of_region exp.at) (Print.string_of_exp exp);
    print_endline @@ Print.string_of_value value;
    Ok ctx
  in
  (match prem.it with
  | IterPr _ -> ()
  | _ ->
      Instrumentation.Dispatcher.emit (Events.Prem_enter { prem; at = prem.at }));
  let result =
    match prem.it with
    | RelPr { relid; notexp } -> eval_rule_prem ctx relid notexp
    | RelAssertPr { call; expect } -> eval_rel_assert_prem ctx call expect
    | IfPr { cond; role } ->
        let fail_with =
          match role with Guard -> fail_guard | Condition -> fail
        in
        eval_cond_prem fail_with ctx cond
    | ElsePr -> Ok ctx
    | LetPr (exp_l, exp_r) -> eval_let_prem ctx exp_l exp_r
    | IterPr (prem, iterexp) -> eval_iter_prem ctx prem iterexp
    | DebugPr exp -> eval_debug_prem ctx exp
  in
  (match prem.it with
  | IterPr _ -> ()
  | _ ->
      Instrumentation.Dispatcher.emit_on_demand @@ fun () ->
      let ctx_out = Result.value ~default:ctx result in
      let bindings =
        Free.free_prem prem |> Common.Domain.IdSet.elements
        |> List.filter_map (fun id ->
               Ctx.find_value_opt ctx_out ~var:(id, [])
               |> Option.map (fun value -> (id, value)))
      in
      Events.Prem_exit
        { prem; at = prem.at; success = Result.is_ok result; bindings });
  result

and eval_prems (ctx : Ctx.t) (prems : prem list) : Ctx.t attempt =
  List.fold_left
    (fun ctx prem ->
      let* ctx = ctx in
      eval_prem ctx prem)
    (Ok ctx) prems

(* Iterated premise evaluation *)

and eval_iter_prem_opt (ctx : Ctx.t) (prem : prem) (vars : var list) :
    Ctx.t attempt =
  let vars_bound, vars_binding =
    List.partition
      (fun { varid; iters; _ } ->
        Ctx.has_value ctx ~var:(varid, iters @ [ Opt ]))
      vars
  in
  let* ctx_sub_opt = Ctx.for_optional_iteration ctx ~vars:vars_bound in
  match ctx_sub_opt with
  | None ->
      let bindings =
        List.map
          (fun { varid = id_binding; typ = typ_binding; iters = iters_binding }
             ->
            let typ =
              Lang.Il.Typ.iterate typ_binding (iters_binding @ [ Opt ])
            in
            let value_binding = None |> Value.Make.opt typ.it in
            ((id_binding, iters_binding @ [ Opt ]), value_binding))
          vars_binding
      in
      Ok (Ctx.add_values_exn ctx ~bindings)
  | Some ctx_sub ->
      Instrumentation.Dispatcher.emit
        (Events.Iter_prem_enter { prem; at = prem.at });
      let* ctx_sub = eval_prem ctx_sub prem in
      Instrumentation.Dispatcher.emit (Events.Iter_prem_exit { at = prem.at });
      let bindings =
        List.map
          (fun { varid = id_binding; typ = typ_binding; iters = iters_binding }
             ->
            let typ =
              Lang.Il.Typ.iterate typ_binding (iters_binding @ [ Opt ])
            in
            let value_binding =
              Ctx.find_value_exn ctx_sub ~var:(id_binding, iters_binding)
            in
            let value_binding = Some value_binding |> Value.Make.opt typ.it in
            ((id_binding, iters_binding @ [ Opt ]), value_binding))
          vars_binding
      in
      Ok (Ctx.add_values_exn ctx ~bindings)

and eval_iter_prem (ctx : Ctx.t) (prem : prem) (iterexp : iterexp) :
    Ctx.t attempt =
  (* List iteration *)
  let eval_iter_prem_list ctx prem vars =
    (* Discriminate between bound and binding variables *)
    let vars_bound, vars_binding =
      List.partition
        (fun { varid; iters; _ } ->
          Ctx.has_value ctx ~var:(varid, iters @ [ List ]))
        vars
    in
    (* Create a subcontext for each batch of bound values *)
    let* ctxs_sub = Ctx.for_list_iteration ctx ~vars:vars_bound in
    let* ctx, values_binding =
      match ctxs_sub with
      (* If the bound variable supposed to guide the iteration is already empty,
         then the binding variables are also empty *)
      | [] ->
          let values_binding =
            List.init (List.length vars_binding) (fun _ -> [])
          in
          Ok (ctx, values_binding)
      (* Otherwise, evaluate the premise for each batch of bound values,
       and collect the resulting binding batches *)
      | _ ->
          (* Hook: iteration start *)
          let* ctx, values_binding_batch_rev =
            List.fold_left
              (fun ctx_values_binding_batch ctx_sub ->
                let* ctx, values_binding_batch_rev = ctx_values_binding_batch in
                Instrumentation.Dispatcher.emit
                  (Events.Iter_prem_enter { prem; at = prem.at });
                let* ctx_sub = eval_prem ctx_sub prem in
                Instrumentation.Dispatcher.emit
                  (Events.Iter_prem_exit { at = prem.at });
                let value_binding_batch =
                  List.map
                    (fun { varid = id_binding; iters = iters_binding; _ } ->
                      Ctx.find_value_exn ctx_sub ~var:(id_binding, iters_binding))
                    vars_binding
                in
                let values_binding_batch_rev =
                  value_binding_batch :: values_binding_batch_rev
                in
                Ok (ctx, values_binding_batch_rev))
              (Ok (ctx, []))
              ctxs_sub
          in
          let* values_binding =
            values_binding_batch_rev |> List.rev
            |> Interp_runtime.Context.transpose_values
          in
          Ok (ctx, values_binding)
    in
    (* Finally, bind the resulting binding batches *)
    let ctx =
      List.fold_left2
        (fun ctx
             { varid = id_binding; typ = typ_binding; iters = iters_binding }
             values_binding ->
          let value_binding =
            let typ =
              Lang.Il.Typ.iterate typ_binding (iters_binding @ [ List ])
            in
            values_binding |> Value.Make.list typ.it
          in
          Ctx.add_value_exn ctx
            ~var:(id_binding, iters_binding @ [ List ])
            ~value:value_binding)
        ctx vars_binding values_binding
    in
    Ok ctx
  in
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_prem_opt ctx prem vars
  | List -> eval_iter_prem_list ctx prem vars

(* Invoke a relation *)

and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list) :
    value list attempt =
  Instrumentation.Dispatcher.emit
    (Events.Rel_enter { id = id.it; at = id.at; inputs = values_input });
  (* Rule matching *)
  let match_rule ctx reltyp rule values_input =
    let { concl; prems; _ } = rule.it in
    let exps_input, exps_output =
      Mode.partition reltyp.it (Mixfix.args concl)
    in
    checkf
      (List.length exps_input = List.length values_input)
      rule.at "arity mismatch in rule";
    let ctx = assign_exps ctx exps_input values_input in
    (ctx, prems, exps_output)
  in
  let reltyp, rules = Ctx.find_rel_exn ctx ~id in
  let attempt_of_rule rule =
    let { ruleid = id_rule; _ } = rule.it in
    let attempt_rule' (ctx_local : Ctx.t) (prems : prem list)
        (exps_output : exp list) : value list attempt =
      let* ctx_local = eval_prems ctx_local prems in
      let values_output = eval_exps ctx_local exps_output in
      Ok values_output
    in
    let attempt_rule () : value list attempt =
      Instrumentation.Dispatcher.emit
        (Events.Rule_enter { id = id.it; rule_id = id_rule.it; at = id.at });
      (* Create a subtrace for the rule *)
      let ctx_local = Ctx.for_call ctx in
      (* Try to match the rule *)
      let ctx_local, prems, exps_output =
        match_rule ctx_local reltyp rule values_input
      in
      (* Try evaluating the rule *)
      let result =
        attempt_rule' ctx_local prems exps_output
        |> nest id.at
             (F.asprintf "application of rule %s/%s failed" id.it id_rule.it)
      in
      Instrumentation.Dispatcher.emit
        (Events.Rule_exit
           {
             id = id.it;
             rule_id = id_rule.it;
             at = id.at;
             success = Result.is_ok result;
           });
      result
    in
    attempt_rule
  in
  (* Main invocation logic *)
  let invoke_rel' () =
    check_warnf (rules <> []) id.at "relation has no rules";
    let attempt_rules () =
      let rules_fallback, rules_ordinary =
        List.partition is_fallback_rule rules
      in
      let attempt_ordinary () =
        if Ctx.has_determinism_checks ctx then
          let labelled rule = (rule, attempt_of_rule rule) in
          match choice_unique (List.map labelled rules_ordinary) with
          | Unique (_, result) -> Ok result
          | Overlapping (rule_l, rule_r) ->
              error id.at
                (F.asprintf "relation %s applies through both %s and %s" id.it
                   (describe_rule id rule_l) (describe_rule id rule_r))
          | Unmatched failtraces -> Error failtraces
        else choice (List.map attempt_of_rule rules_ordinary)
      in
      match attempt_ordinary () with
      | Ok result -> Ok result
      | Error failtraces ->
          choice_fallback failtraces (List.map attempt_of_rule rules_fallback)
    in
    Ctx.memoize_rel ctx ~id ~values:values_input ~f:attempt_rules
  in
  let result = invoke_rel' () in
  Instrumentation.Dispatcher.emit_on_demand (fun () ->
      let outs =
        match result with
        | Ok values_output -> List.map Option.some values_output
        | Error _ -> List.map (fun _ -> None) (Mode.outputs reltyp.it)
      in
      let conclusion = Mode.fill reltyp.it ~ins:values_input ~outs in
      Events.Rel_exit
        { id = id.it; at = id.at; success = Result.is_ok result; conclusion });
  result |> nest id.at (F.asprintf "invocation of relation %s failed" id.it)

(* Invoke a function *)

and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    value attempt =
  let values_input = eval_args ctx args in
  Instrumentation.Dispatcher.emit
    (Events.Func_enter { id = id.it; at = id.at; inputs = values_input });
  (* Clause matching *)
  let match_clause ctx_caller ctx_callee clause values_input =
    let { args = args_input; body = exp_output; prems } = clause.it in
    checkf
      (List.length args_input = List.length values_input)
      clause.at "arity mismatch while matching clause";
    let ctx = assign_args ctx_caller ctx_callee args_input values_input in
    (ctx, args_input, prems, exp_output)
  in
  (* Builtin function invocation *)
  let invoke_func_builtin signature =
    (* Invoke builtin function *)
    let invoke_func_builtin' () =
      Instrumentation.Dispatcher.emit
        (Events.Clause_enter { id = id.it; clause_idx = 0; at = id.at });
      let value_output =
        Ctx.call_builtin ctx ~id ~targs ~values:values_input |> unwrap_builtin
      in
      (match
         Builtins.check_result ~opaque_types:(Ctx.opaque_types ctx)
           ~find_typdef_opt:(fun id -> Ctx.find_typdef_opt ctx ~id)
           ~signature ~targs value_output
       with
      | Ok () -> ()
      | Error message ->
          error id.at ("builtin $" ^ id.it ^ " result: " ^ message));
      Instrumentation.Dispatcher.emit
        (Events.Clause_exit
           { id = id.it; clause_idx = 0; at = id.at; success = true });
      Ok value_output
    in
    invoke_func_builtin' ()
  in
  (* User-defined function invocation *)
  let invoke_func_def tparams clauses =
    (* Evaluate type arguments *)
    let targs =
      match targs with
      | [] -> []
      | targs ->
          let theta =
            Ctx.fold_local_typdefs ctx ~init:TIdMap.empty
              ~f:(fun theta tid typdef ->
                let tparams, deftyp = typdef in
                match (tparams, deftyp.it) with
                | [], PlainT typ -> TIdMap.add tid typ theta
                | _ -> theta)
          in
          List.map (Typ.subst_typ theta) targs
    in
    let attempt_of_clause idx_clause clause =
      let attempt_clause' (ctx_local : Ctx.t) (prems : prem list)
          (exp_output : exp) : value attempt =
        let* ctx_local = eval_prems ctx_local prems in
        let value_output = eval_exp ctx_local exp_output in
        Ok value_output
      in
      let attempt_clause () : value attempt =
        Instrumentation.Dispatcher.emit
          (Events.Clause_enter
             { id = id.it; clause_idx = idx_clause; at = id.at });
        (* Create a subtrace for the clause *)
        let ctx_local = Ctx.for_call ctx in
        (* Add type arguments to the context *)
        checkf
          (List.length targs = List.length tparams)
          id.at "arity mismatch in type arguments";
        let typdef_bindings =
          List.map2
            (fun tparam targ -> (tparam, ([], PlainT targ $ targ.at)))
            tparams targs
        in
        let ctx_local =
          Ctx.add_typdefs_exn ctx_local ~bindings:typdef_bindings
        in
        (* Try to match the clause *)
        let ctx_local, args_input, prems, exp_output =
          match_clause ctx ctx_local clause values_input
        in
        (* Try evaluating the clause *)
        let result =
          attempt_clause' ctx_local prems exp_output
          |> nest id.at
               (F.asprintf "application of clause %s%s failed" id.it
                  (Print.string_of_args args_input))
        in
        Instrumentation.Dispatcher.emit
          (Events.Clause_exit
             {
               id = id.it;
               clause_idx = idx_clause;
               at = id.at;
               success = Result.is_ok result;
             });
        result
      in
      attempt_clause
    in
    let attempt_clauses () =
      let clauses_fallback, clauses_ordinary =
        List.mapi (fun idx_clause clause -> (idx_clause, clause)) clauses
        |> List.partition (fun (_, clause) -> is_fallback_clause clause)
      in
      let attempt_of_indexed (idx_clause, clause) =
        attempt_of_clause idx_clause clause
      in
      let attempt_ordinary () =
        if Ctx.has_determinism_checks ctx then
          let labelled (idx_clause, clause) =
            (idx_clause, attempt_of_clause idx_clause clause)
          in
          match choice_unique (List.map labelled clauses_ordinary) with
          | Unique (_, result) -> Ok result
          | Overlapping (idx_clause_l, idx_clause_r) ->
              error id.at
                (F.asprintf
                   "function %s applies through both clause %d and clause %d"
                   id.it idx_clause_l idx_clause_r)
          | Unmatched failtraces -> Error failtraces
        else choice (List.map attempt_of_indexed clauses_ordinary)
      in
      match attempt_ordinary () with
      | Ok result -> Ok result
      | Error failtraces ->
          choice_fallback failtraces
            (List.map attempt_of_indexed clauses_fallback)
    in
    attempt_clauses ()
  in
  (* Main dispatch *)
  let result =
    let invoke_func' () =
      match Ctx.find_func_opt ctx ~id with
      | Some (Ctx.Func.Builtin (tparams, typ)) ->
          checkf (Ctx.has_builtin ctx ~id) id.at
            "builtin $%s is declared in the spec but not implemented" id.it;
          invoke_func_builtin (tparams, typ)
      | Some (Ctx.Func.Defined (tparams, clauses)) ->
          check_warnf (clauses <> []) id.at "function has no clauses";
          invoke_func_def tparams clauses
      | None ->
          if Ctx.has_builtin ctx ~id then
            error id.at
              (F.asprintf "builtin $%s is invoked without a spec declaration"
                 id.it)
          else fail id.at (F.asprintf "unknown function %s" id.it)
    in
    let has_function_arguments values =
      List.exists
        (fun value ->
          match value.it with Lang.Il.FuncV _ -> true | _ -> false)
        values
    in
    (* Cache keys omit type arguments and local function definitions. *)
    if
      targs <> [] || Ctx.has_local_func ctx ~id
      || has_function_arguments values_input
    then invoke_func' ()
    else Ctx.memoize_func ctx ~id ~values:values_input ~f:invoke_func'
  in
  let output = match result with Ok v -> Some v | Error _ -> None in
  Instrumentation.Dispatcher.emit
    (Events.Func_exit { id = id.it; at = id.at; output });
  result
  |> nest id.at
       (F.asprintf "invocation of function %s%s%s failed"
          (Print.string_of_defid id)
          (Print.string_of_targs targs)
          (Print.string_of_args args))

(* Load definitions into the context *)

let global_definition_of_def ~opaque_types (def : def) : Ctx.Global.definition =
  match def.it with
  | TypD { synid = id; tparams; deftyp } ->
      (match Opaque_types.check_declaration opaque_types ~id ~deftyp with
      | Ok () -> ()
      | Error message -> error id.at message);
      Ctx.Global.Syntax (id, (tparams, deftyp))
  | RelD { relid = id; reltyp; rules } ->
      Ctx.Global.Relation (id, (reltyp, rules))
  | BuiltinDecD { defid = id; tparams; typ; _ } ->
      Ctx.Global.Function (id, Ctx.Func.Builtin (tparams, typ))
  | DecD { defid = id; tparams; clauses; _ } ->
      Ctx.Global.Function (id, Ctx.Func.Defined (tparams, clauses))

let context_of_spec (spec : spec) ~builtins ~cache ~opaque_types
    ~check_determinism : Ctx.t =
  let definitions = List.map (global_definition_of_def ~opaque_types) spec in
  let global = Ctx.Global.of_definitions_exn definitions in
  Ctx.make global ~builtins ~cache ~opaque_types ~check_determinism
