open Common.Annotated
module Il = Lang.Il
module Al = Lang.Al
open Lang.Il
open Lang.Al
module Mixop = Lang.Il.Mixfix

type error = Diagnostic.error

(* A failed input [Guard] makes the rule or clause inapplicable. *)
let rec as_guard (instr : Al.instr) : Al.instr =
  let guard =
    match instr.it with
    | IfI { cond; _ } -> IfI { cond; role = Guard }
    | IterI (instr_inner, iterinstr) -> IterI (as_guard instr_inner, iterinstr)
    | _ -> instr.it
  in
  guard $$ (instr.at, instr.note)

(* Expression analysis *)

let analyze_exps_as_bind (exps : exp list) ~(dctx : Dctx.t) ~(origin : origin) :
    Dctx.t * exp list * Al.instr list =
  let dctx, venv, exps, sideconditions =
    Binding.analyze_exps_as_bind exps ~dctx ~origin
  in
  let dctx = Dctx.add_bounds dctx venv in
  let exps = Dimension.analyze_exps dctx.bounds exps in
  let sideconditions = Dimension.analyze_instrs dctx sideconditions in
  (dctx, exps, sideconditions)

let analyze_exp_as_bound (dctx : Dctx.t) (exp : exp) : exp =
  Binding.analyze_exp_as_bound dctx exp;
  Dimension.analyze_exp dctx.bounds exp

let analyze_exps_as_bound (dctx : Dctx.t) (exps : exp list) : exp list =
  List.map (analyze_exp_as_bound dctx) exps

(* Argument analysis *)

let analyze_args_as_bind (args : arg list) ~(dctx : Dctx.t) ~(origin : origin) :
    Dctx.t * arg list * Al.instr list =
  let dctx, venv, args, sideconditions =
    Binding.analyze_args_as_bind args ~dctx ~origin
  in
  let dctx = Dctx.add_bounds dctx venv in
  let args = Dimension.analyze_args dctx.bounds args in
  let sideconditions = Dimension.analyze_instrs dctx sideconditions in
  (dctx, args, sideconditions)

(* Premise analysis *)

let analyze_prem (dctx : Dctx.t) (prem : Il.prem) : Dctx.t * Al.instr list =
  let dctx, venv, instr, sideconditions = Binding.analyze_prem prem ~dctx in
  let dctx = Dctx.add_bounds dctx venv in
  let instr = Dimension.analyze_instr dctx instr in
  let sideconditions = Dimension.analyze_instrs dctx sideconditions in
  (dctx, instr :: sideconditions)

let analyze_prems (dctx : Dctx.t) (prems : Il.prem list) :
    Dctx.t * Al.instr list =
  let dctx, instrs = List.fold_left_map analyze_prem dctx prems in
  (dctx, List.concat instrs)

(* Rule analysis *)

let analyze_rule (env : Dctx.env) ~(reltyp : reltyp) (rule : Il.rule) : Al.rule
    =
  let ({ ruleid; concl; prems } : Il.rule') = rule.it in
  let dctx = Dctx.make env ~frees:(Il.Free.free_rule rule) in
  let mixop, exps = Mixop.split concl in
  let exps_input, exps_output = Mode.partition reltyp.it exps in
  let dctx, exps_input, sideconditions =
    analyze_exps_as_bind exps_input ~dctx ~origin:Input_sidecondition
  in
  let dctx, instrs = analyze_prems dctx prems in
  let instrs = List.map as_guard sideconditions @ instrs in
  let exps_output = analyze_exps_as_bound dctx exps_output in
  let concl =
    Mode.interleave reltyp.it ~ins:exps_input ~outs:exps_output
    |> Mixop.fill mixop
  in
  { ruleid; concl; instrs } $ rule.at

(* Clause analysis *)

let analyze_clause (env : Dctx.env) (clause : Il.clause) : Al.clause =
  let ({ args; body; prems } : Il.clause') = clause.it in
  let dctx = Dctx.make env ~frees:(Il.Free.free_clause clause) in
  let dctx, args, sideconditions =
    analyze_args_as_bind args ~dctx ~origin:Input_sidecondition
  in
  let dctx, instrs = analyze_prems dctx prems in
  let instrs = List.map as_guard sideconditions @ instrs in
  let body = analyze_exp_as_bound dctx body in
  { args; body; instrs } $ clause.at

(* Spec analysis *)

let map_collecting_errors ~(f : 'a -> 'b) (constructs : 'a list) :
    'b list * Diag.t list =
  List.partition_map
    (fun construct ->
      match f construct with
      | result -> Either.Left result
      | exception Diagnostic.AlgoError e -> Either.Right e)
    constructs

let analyze_def (env : Dctx.env) (def : Il.def) : Al.def * Diag.t list =
  match def.it with
  | Il.TypD { synid; tparams; deftyp } ->
      (Al.TypD { synid; tparams; deftyp } $ def.at, [])
  | Il.RelD { relid; reltyp; rules } ->
      let rules, errors =
        map_collecting_errors ~f:(analyze_rule env ~reltyp) rules
      in
      (Al.RelD { relid; reltyp; rules } $ def.at, errors)
  | Il.DecD { defid; tparams; params; typ; clauses } ->
      let clauses, errors =
        map_collecting_errors ~f:(analyze_clause env) clauses
      in
      (Al.DecD { defid; tparams; params; typ; clauses } $ def.at, errors)
  | Il.BuiltinDecD { defid; tparams; params; typ; hints } ->
      (Al.BuiltinDecD { defid; tparams; params; typ; hints } $ def.at, [])

let algo_spec (spec : Il.spec) : Al.spec * Diag.t list =
  let env = Dctx.make_env spec in
  let spec, errors = List.map (analyze_def env) spec |> List.split in
  (spec, List.concat errors)
