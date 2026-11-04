open Common.Annotated
open Lang.Il
module Mixop = Lang.Il.Mixfix

(* Entry point of dataflow analysis *)

(* Re-tag the checks synthesized from a rule/clause input pattern as guards. *)
let rec as_guard (prem : prem) : prem =
  let guard =
    match prem.it with
    | IfPr { cond; _ } -> IfPr { cond; role = Guard }
    | IterPr (prem_inner, iterinstr) -> IterPr (as_guard prem_inner, iterinstr)
    | _ -> prem.it
  in
  guard $$ (prem.at, Some Synthesized)

let as_synthesized (prem : prem) : prem = prem.it $$ (prem.at, Some Synthesized)

(* Expression analysis *)

let analyze_exps_as_bind (dctx : Dctx.t) (exps : exp list) :
    Dctx.t * exp list * prem list =
  let dctx, venv, exps, sideconditions =
    Binding.analyze_exps_as_bind dctx exps
  in
  let dctx = Dctx.add_bounds dctx venv in
  let exps = Dimension.analyze_exps dctx.bounds exps in
  let sideconditions = Dimension.analyze_prems dctx sideconditions in
  (dctx, exps, sideconditions)

let analyze_exp_as_bound (dctx : Dctx.t) (exp : exp) : exp =
  Binding.analyze_exp_as_bound dctx exp;
  Dimension.analyze_exp dctx.bounds exp

let analyze_exps_as_bound (dctx : Dctx.t) (exps : exp list) : exp list =
  List.map (analyze_exp_as_bound dctx) exps

(* Argument analysis *)

let analyze_args_as_bind (dctx : Dctx.t) (args : arg list) :
    Dctx.t * arg list * prem list =
  let dctx, venv, args, sideconditions =
    Binding.analyze_args_as_bind dctx args
  in
  let dctx = Dctx.add_bounds dctx venv in
  let args = Dimension.analyze_args dctx.bounds args in
  let sideconditions = Dimension.analyze_prems dctx sideconditions in
  (dctx, args, sideconditions)

(* Premise analysis *)

let analyze_prem (dctx : Dctx.t) (prem : prem) : Dctx.t * prem list =
  let dctx, venv, prem_analyzed, sideconditions =
    Binding.analyze_prem dctx prem
  in
  let dctx = Dctx.add_bounds dctx venv in
  let prem_analyzed = Dimension.analyze_prem dctx prem_analyzed in
  let prem_analyzed = prem_analyzed.it $$ (prem_analyzed.at, prem.note) in
  let sideconditions =
    Dimension.analyze_prems dctx sideconditions |> List.map as_synthesized
  in
  (dctx, prem_analyzed :: sideconditions)

let analyze_prems (dctx : Dctx.t) (prems : prem list) : Dctx.t * prem list =
  let dctx, prems = List.fold_left_map analyze_prem dctx prems in
  (dctx, List.concat prems)

(* Rule analysis *)

let analyze_rule (env : Dctx.env) ~(reltyp : reltyp) (rule : rule) : rule =
  let { ruleid; concl; prems } = rule.it in
  let dctx = Dctx.make env ~frees:(Free.free_rule rule) in
  let mixop, exps = Mixop.split concl in
  let exps_input, exps_output = Mode.partition reltyp.it exps in
  let dctx, exps_input, sideconditions = analyze_exps_as_bind dctx exps_input in
  let dctx, prems = analyze_prems dctx prems in
  let prems = List.map as_guard sideconditions @ prems in
  let exps_output = analyze_exps_as_bound dctx exps_output in
  let concl =
    Mode.interleave reltyp.it ~ins:exps_input ~outs:exps_output
    |> Mixop.fill mixop
  in
  { ruleid; concl; prems } $ rule.at

(* Clause analysis *)

let analyze_clause (env : Dctx.env) (clause : clause) : clause =
  let { args; body; prems } = clause.it in
  let dctx = Dctx.make env ~frees:(Free.free_clause clause) in
  let dctx, args, sideconditions = analyze_args_as_bind dctx args in
  let dctx, prems = analyze_prems dctx prems in
  let prems = List.map as_guard sideconditions @ prems in
  let body = analyze_exp_as_bound dctx body in
  { args; body; prems } $ clause.at

(* Spec analysis *)

let map_collecting_errors ~(f : 'a -> 'a) (constructs : 'a list) :
    'a list * Diag.t list =
  List.partition_map
    (fun construct ->
      match f construct with
      | result -> Either.Left result
      | exception Diagnostic.ElabError e -> Either.Right e)
    constructs

let analyze_def (env : Dctx.env) (def : def) : def * Diag.t list =
  match def.it with
  | RelD { relid; reltyp; rules } ->
      let rules, errors =
        map_collecting_errors ~f:(analyze_rule env ~reltyp) rules
      in
      (RelD { relid; reltyp; rules } $ def.at, errors)
  | DecD { defid; tparams; params; typ; clauses } ->
      let clauses, errors =
        map_collecting_errors ~f:(analyze_clause env) clauses
      in
      (DecD { defid; tparams; params; typ; clauses } $ def.at, errors)
  | TypD _ | BuiltinDecD _ -> (def, [])

let analyze_spec (spec : spec) : spec * Diag.t list =
  let env = Dctx.make_env spec in
  let spec, errors = List.map (analyze_def env) spec |> List.split in
  (spec, List.concat errors)
