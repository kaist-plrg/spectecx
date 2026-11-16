open Common.Domain
open Common.Annotated
module Location = Common.Location
module Il = Lang.Il
open Lang.Il.Expression
open Lang.Il
open Lang.Al
open Diagnostic
open Dctx
open Bind
module Mixop = Lang.Il.Mixfix

(* Binding analysis :

   1. Collect all binding occurrences of variables in IL construct
      - Check that all binding occurrences reside in invertible constructs
   2. Rename multi/parallel binding occurrences
      - e.g., -- let (int, int) = ... becomes
                -- let (int, int') = ..., -- if int = int'
   3. Desugar partial bindings, occurring as either:
      (1) Bound values occurring inside binder patterns
          - e.g., -- let PATTERN (a, 1 + 2) = ... becomes
                  -- let PATTERN (a, int) = ..., -- if int == 1 + 2
      (2) Injection of a variant case
          - e.g., -- let PATTERN (a, int) = pat becomes
                  -- if pat matches PATTERN, -- let PATTERN (a, b) = pat
      (3) Injection of a subtype case
          - e.g., -- let ((typ) child) = parent becomes
                  -- if parent <: child, -- let child = parent as child
   Note. At this point, binder patterns are one of:
      - VarE, TupleE, CaseE of a singleton case, StrE
      - IterE of the above cases *)

let update_venv_multi (venv : VEnv.t) (renv_multi : Multibind.REnv.t) : VEnv.t =
  Multibind.REnv.fold
    (fun id ids_rename venv ->
      let ids_rename = IdSet.elements ids_rename in
      let typ = VEnv.find id venv in
      List.fold_left
        (fun venv id_rename -> VEnv.add id_rename typ venv)
        venv ids_rename)
    renv_multi venv

let update_venv_partial (venv : VEnv.t) (renv_partial : Partialbind.REnv.t) :
    VEnv.t =
  List.fold_left
    (fun venv (to_, _, iters) ->
      let iters = to_.iters @ iters in
      VEnv.add to_.varid (to_.typ, iters) venv)
    venv renv_partial

(* Expression binding analysis *)

let analyze_exps_as_bind (exps : exp list) ~(dctx : Dctx.t) ~(origin : origin) :
    Dctx.t * VEnv.t * exp list * instr list =
  let binds = Collectbind.collect_exps dctx exps in
  let venv = BEnv.flatten binds in
  let dctx, renv_multi, exps =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_exps dctx renv_multi exps
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions renv_multi ~binds ~origin
  in
  let dctx, renv_partial, exps =
    Partialbind.rename_exps dctx (VEnv.dom venv) Partialbind.REnv.empty exps
  in
  let venv = update_venv_partial venv renv_partial in
  let sideconditions_partial =
    Partialbind.gen_sideconditions renv_partial ~dctx ~origin
  in
  let sideconditions = sideconditions_partial @ sideconditions_multi in
  (dctx, venv, exps, sideconditions)

let analyze_exp_as_bound (dctx : Dctx.t) (exp : exp) : unit =
  let binds = Collectbind.collect_exp dctx exp in
  if not (BEnv.is_empty binds) then
    error exp.at
      (Format.asprintf "expression has free variable(s): %s"
         (BEnv.to_string binds))
      ~code:Free_variable_in_output
      ~detail:
        "Every variable here must already be bound by an earlier part of the \
         rule (the conclusion's input slot or a preceding premise)."

let analyze_exps_as_bound (dctx : Dctx.t) (exps : exp list) : unit =
  List.iter (analyze_exp_as_bound dctx) exps

(* Argument binding analysis *)

let analyze_args_as_bind (args : arg list) ~(dctx : Dctx.t) ~(origin : origin) :
    Dctx.t * VEnv.t * arg list * instr list =
  let binds = Collectbind.collect_args dctx args in
  let venv = BEnv.flatten binds in
  let dctx, renv_multi, args =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_args dctx renv_multi args
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions renv_multi ~binds ~origin
  in
  let dctx, renv_partial, args =
    Partialbind.rename_args dctx (VEnv.dom venv) Partialbind.REnv.empty args
  in
  let venv = update_venv_partial venv renv_partial in
  let sideconditions_partial =
    Partialbind.gen_sideconditions renv_partial ~dctx ~origin
  in
  let sideconditions = sideconditions_partial @ sideconditions_multi in
  (dctx, venv, args, sideconditions)

(* Premise binding analysis *)

let rec analyze_prem (prem : Il.prem) ~(dctx : Dctx.t) :
    Dctx.t * VEnv.t * Al.instr * Al.instr list =
  match prem.it with
  | Il.RelPr { relid; notexp } -> analyze_rel_prem prem ~dctx ~relid ~notexp
  | Il.RelAssertPr { call; expect } ->
      analyze_rel_assert_prem prem ~dctx ~call ~expect
  | Il.IfPr cond -> analyze_if_prem prem ~dctx ~cond
  | Il.ElsePr -> (dctx, VEnv.empty, OtherwiseI $$ (prem.at, Premise prem), [])
  | Il.IterPr (prem_inner, iter) ->
      analyze_iter_prem prem ~dctx ~prem_inner ~iter
  | Il.DebugPr exp -> analyze_debug_prem prem ~dctx ~exp

and analyze_rel_prem (prem : Il.prem) ~(dctx : Dctx.t) ~(relid : id)
    ~(notexp : notexp) : Dctx.t * VEnv.t * Al.instr * Al.instr list =
  let mixop, exps = Mixop.split notexp in
  let reltyp = Dctx.find_reltyp dctx relid in
  let exps_input, exps_output = Mode.partition reltyp.it exps in
  analyze_exps_as_bound dctx exps_input;
  let dctx, venv, exps_output, sideconditions =
    analyze_exps_as_bind exps_output ~dctx ~origin:(Sidecondition prem)
  in
  let exps = Mode.interleave reltyp.it ~ins:exps_input ~outs:exps_output in
  let notexp = Mixop.fill mixop exps in
  let instr = RelI { relid; notexp } $$ (prem.at, Premise prem) in
  (dctx, venv, instr, sideconditions)

and analyze_if_eq_prem ~(dctx : Dctx.t) ~(origin : origin) ~(at : Location.t)
    ~(typ : typ') ~(optyp : optyp) ~(exp_l : exp) ~(exp_r : exp) :
    Dctx.t * VEnv.t * instr' * instr list =
  let binds_l = Collectbind.collect_exp dctx exp_l in
  let binds_r = Collectbind.collect_exp dctx exp_r in
  match (BEnv.is_empty binds_l, BEnv.is_empty binds_r) with
  | true, true ->
      let instr =
        IfI
          {
            cond = CmpE (`EqOp, optyp, exp_l, exp_r) $$ (at, { typ; annot = () });
            role = Condition;
          }
      in
      (dctx, VEnv.empty, instr, [])
  | false, true ->
      analyze_let_prem ~dctx ~origin ~exp_bind:exp_l ~binds:binds_l
        ~exp_bound:exp_r
  | true, false ->
      analyze_let_prem ~dctx ~origin ~exp_bind:exp_r ~binds:binds_r
        ~exp_bound:exp_l
  | false, false ->
      error at
        (Format.asprintf
           "cannot bind on both sides of an equality: (left) %s, (right) %s"
           (BEnv.to_string binds_l) (BEnv.to_string binds_r))
        ~code:Bind_both_sides_of_equality
        ~detail:
          "An `=` premise reads as a comparison when both sides are already \
           bound, or as a binder when one side is. With new variables on both \
           sides it fits neither."

and analyze_if_prem (prem : Il.prem) ~(dctx : Dctx.t) ~(cond : exp) :
    Dctx.t * VEnv.t * Al.instr * Al.instr list =
  match cond.it with
  | CmpE (`EqOp, optyp, exp_l, exp_r) ->
      let dctx, venv, instr, sideconditions =
        analyze_if_eq_prem ~dctx ~origin:(Sidecondition prem) ~at:cond.at
          ~typ:cond.note.typ ~optyp ~exp_l ~exp_r
      in
      (dctx, venv, instr $$ (prem.at, Premise prem), sideconditions)
  | _ ->
      analyze_exp_as_bound dctx cond;
      let instr = IfI { cond; role = Condition } $$ (prem.at, Premise prem) in
      (dctx, VEnv.empty, instr, [])

and analyze_rel_assert_prem (prem : Il.prem) ~(dctx : Dctx.t) ~(call : relcall)
    ~(expect : bool) : Dctx.t * VEnv.t * Al.instr * Al.instr list =
  analyze_exps_as_bound dctx (Mixop.args call.notexp);
  let instr = RelAssertI { call; expect } $$ (prem.at, Premise prem) in
  (dctx, VEnv.empty, instr, [])

and analyze_let_prem ~(dctx : Dctx.t) ~(origin : origin) ~(exp_bind : exp)
    ~(binds : BEnv.t) ~(exp_bound : exp) : Dctx.t * VEnv.t * instr' * instr list
    =
  let venv = BEnv.flatten binds in
  let dctx, renv_multi, exp_bind =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_exp dctx renv_multi exp_bind
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions renv_multi ~binds ~origin
  in
  let dctx, renv_partial, exp_bind =
    Partialbind.rename_exp dctx (VEnv.dom venv) Partialbind.REnv.empty exp_bind
  in
  let venv = update_venv_partial venv renv_partial in
  let sideconditions_partial =
    Partialbind.gen_sideconditions renv_partial ~dctx ~origin
  in
  let sideconditions = sideconditions_partial @ sideconditions_multi in
  (dctx, venv, LetI (exp_bind, exp_bound), sideconditions)

and analyze_iter_prem (prem : Il.prem) ~(dctx : Dctx.t) ~(prem_inner : Il.prem)
    ~(iter : iter) : Dctx.t * VEnv.t * Al.instr * Al.instr list =
  let at = prem_inner.at in
  let dctx, venv, instr_inner, sideconditions = analyze_prem prem_inner ~dctx in
  let venv = VEnv.map (Typ.add_iter iter) venv in
  let sideconditions =
    List.map
      (fun sidecondition ->
        IterI (sidecondition, Dimension.unannotated_iterinstr iter)
        $$ (at, Sidecondition prem))
      sideconditions
  in
  let instr =
    IterI (instr_inner, Dimension.unannotated_iterinstr iter)
    $$ (at, Premise prem)
  in
  (dctx, venv, instr, sideconditions)

and analyze_debug_prem (prem : Il.prem) ~(dctx : Dctx.t) ~(exp : exp) :
    Dctx.t * VEnv.t * Al.instr * Al.instr list =
  analyze_exp_as_bound dctx exp;
  let instr = DebugI exp $$ (prem.at, Premise prem) in
  (dctx, VEnv.empty, instr, [])
