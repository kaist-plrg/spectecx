open Common.Source
open Lang.Il
module El = Lang.El

(* Structural translation of El.plaintyp → Il.typ.
   No context needed: purely syntactic.
   Type-argument count checking happens in elab_prems_in_spec. *)
let rec elab_plaintyp (pt : El.plaintyp) : typ =
  elab_plaintyp' pt.it $ pt.at

and elab_plaintyp' (pt' : El.plaintyp') : typ' =
  match pt' with
  | El.BoolT          -> BoolT
  | El.NumT t         -> NumT t
  | El.TextT          -> TextT
  | El.ParenT inner   -> elab_plaintyp' inner.it
  | El.TupleT ts      -> TupleT (List.map elab_plaintyp ts)
  | El.IterT (t, El.Opt)  -> IterT (elab_plaintyp t, Opt)
  | El.IterT (t, El.List) -> IterT (elab_plaintyp t, List)
  | El.VarT (id, targs) ->
    let il_targs =
      List.map (fun ta -> elab_plaintyp' ta.it $ ta.at) targs
    in
    VarT (id, il_targs)

(* Build a synthetic notexp: a flat list of Mixfix.Arg (VarE id) for each var. *)
let make_notexp (vars : (id' * typ) list) : notexp =
  List.map (fun (id, typ) ->
    Mixfix.Arg { it = VarE (id $ no_region); note = typ.it; at = no_region }
  ) vars

(* Build a synthetic RelD with one rule. *)
let make_synth_rel_def (rel_id : id') (all_vars : (id' * typ) list)
    (n_inputs : int) (prems : prem list) : def =
  let notexp = make_notexp all_vars in
  let inputs = List.init n_inputs Fun.id in
  let rule_id = "rule" $ no_region in
  let rule = (rule_id, notexp, prems) $ no_region in
  (* nottyp is ignored at load time — use a dummy *)
  let nottyp =
    (List.map (fun (_, typ) -> Mixfix.Arg typ) all_vars) $ no_region
  in
  RelD (rel_id $ no_region, nottyp, inputs, [rule]) $ no_region

(* --- output variable pre-declaration ------------------------------------ *)

(* Reverse of elab_plaintyp: Il.typ → El.plaintyp, for VarPr injection. *)
let rec il_typ_to_el_plaintyp (typ : typ) : El.plaintyp =
  (match typ.it with
   | BoolT            -> El.BoolT
   | NumT t           -> El.NumT t
   | TextT            -> El.TextT
   | TupleT ts        -> El.TupleT (List.map il_typ_to_el_plaintyp ts)
   | IterT (t, Opt)   -> El.IterT (il_typ_to_el_plaintyp t, El.Opt)
   | IterT (t, List)  -> El.IterT (il_typ_to_el_plaintyp t, El.List)
   | VarT (id, targs) -> El.VarT (id, List.map il_typ_to_el_plaintyp targs)
   | FuncT -> El.VarT ("func" $ typ.at, []))
  $ typ.at

(* Look up a relation's (nottyp, input-indices) in the IL spec by name. *)
let find_rel_in_spec (spec : spec) (rel_name : string) :
    (nottyp * int list) option =
  List.find_map (fun def ->
    match def.it with
    | RelD (id, nottyp, inputs, _) when id.it = rel_name ->
      Some (nottyp, inputs)
    | _ -> None
  ) spec

(* Extract non-atom EL sub-expressions from a notexp in argument order,
   mirroring the structural decomposition of elab_exp_not_inner. *)
let rec collect_el_args (exp : El.exp) : El.exp list =
  match exp.it with
  | El.AtomE _           -> []
  | El.SeqE exps         -> List.concat_map collect_el_args exps
  | El.InfixE (l, _, r)  -> collect_el_args l @ collect_el_args r
  | El.BrackE (_, e, _)  -> collect_el_args e
  | El.ParenE e          -> collect_el_args e
  | _                    -> [exp]

(* For one EL premise, return (id, el_plaintyp) pairs for named output
   variables (VarE id where id ≠ "_") found in relation output positions. *)
let rec output_vars_of_el_prem (spec : spec) (prem : El.prem) :
    (El.id * El.plaintyp) list =
  match prem.it with
  | El.RulePr (rel_id, exp) ->
    (match find_rel_in_spec spec rel_id.it with
     | None -> []
     | Some (nottyp, inputs) ->
       let arg_types = Mixfix.args nottyp.it in
       let el_args   = collect_el_args exp in
       let n = min (List.length arg_types) (List.length el_args) in
       List.init n Fun.id
       |> List.filter (fun i -> not (List.mem i inputs))
       |> List.filter_map (fun i ->
          match List.nth_opt el_args i with
          | Some { it = El.VarE id; _ } when id.it <> "_" ->
            Some (id, il_typ_to_el_plaintyp (List.nth arg_types i))
          | _ -> None))
  | El.IterPr (inner, _) -> output_vars_of_el_prem spec inner
  | _ -> []

(* Prepend VarPr premises for any named output variables in [prems] that are
   not already declared in [var_decls].  VarPr produces no IL premise, so
   existing index arithmetic in elab_block is unaffected. *)
let prepend_output_varpr_prems (spec : spec)
    (var_decls : (El.id * El.plaintyp) list)
    (prems : El.prem list) : El.prem list =
  let known = ref (List.map (fun (id, _) -> id.it) var_decls) in
  let extra = ref [] in
  List.iter (fun prem ->
    List.iter (fun (id, plaintyp) ->
      if not (List.mem id.it !known) then begin
        known := id.it :: !known;
        extra := (El.VarPr (id, plaintyp) $ no_region) :: !extra
      end
    ) (output_vars_of_el_prem spec prem)
  ) prems;
  List.rev !extra @ prems

(* --- block elaboration ------------------------------------------------ *)

let elab_block (spec_il : spec) (block : Qc_ast.ast_block) :
    (Qc_ir.qc_command * def list, string) result =
  let name = block.name in
  let free_vars =
    List.map
      (fun p ->
        { Qc_ir.iv_id = p.Qc_ast.p_id.it;
          iv_typ = elab_plaintyp p.Qc_ast.p_typ })
      block.params
  in
  let var_decls =
    List.map (fun p -> (p.Qc_ast.p_id, p.Qc_ast.p_typ)) block.params
  in
  let generator =
    match block.hint with
    | Some (Qc_ast.GeneratorHint n) -> Some n
    | None -> None
  in
  match block.goal with
  | Some goal ->
    (* Property mode: prems are filters, goal is the property to check. *)
    let prems = block.prems in
    (* Elaborate prems alone to get only prems-bound output vars.
       The goal may introduce locals (e.g. wildcards → _0) that must NOT
       appear as outputs of the prems relation. *)
    let prems_with_decls =
      prepend_output_varpr_prems spec_il var_decls prems
    in
    (match Elaborate.elab_prems_in_spec spec_il var_decls prems_with_decls with
     | Error e -> Error (Elaborate.error_to_string e)
     | Ok (il_prems, prems_output_vars) ->
       (* Elaborate prems @ [goal] together so the goal sees prems-bound vars. *)
       let all_with_decls =
         prepend_output_varpr_prems spec_il var_decls (prems @ [goal])
       in
       match Elaborate.elab_prems_in_spec spec_il var_decls all_with_decls with
       | Error e -> Error (Elaborate.error_to_string e)
       | Ok (il_all, _) ->
         let il_goal =
           match List.nth_opt il_all (List.length il_prems) with
           | None -> failwith (Printf.sprintf "quickcheck/%s: goal not found after elaboration" name)
           | Some g -> g
         in
         let input_ids = List.map (fun v -> v.Qc_ir.iv_id) free_vars in
         let input_pairs =
           List.map (fun v -> (v.Qc_ir.iv_id, v.Qc_ir.iv_typ)) free_vars
         in
         let prems_rel_id = Printf.sprintf "__qc_%s_prems__" name in
         let prems_all_vars = input_pairs @ prems_output_vars in
         let prems_def =
           make_synth_rel_def prems_rel_id prems_all_vars
             (List.length free_vars) il_prems
         in
         let prems_rel = Qc_ir.{
           sr_id      = prems_rel_id;
           sr_inputs  = input_ids;
           sr_outputs = prems_output_vars;
         } in
         let goal_rel_id = Printf.sprintf "__qc_%s_goal__" name in
         let goal_all_vars = prems_all_vars in
         let goal_def =
           make_synth_rel_def goal_rel_id goal_all_vars
             (List.length goal_all_vars) [il_goal]
         in
         let goal_input_ids = List.map fst goal_all_vars in
         let goal_rel = Qc_ir.{
           sr_id      = goal_rel_id;
           sr_inputs  = goal_input_ids;
           sr_outputs = [];
         } in
         Ok
           (Qc_ir.QcProp { name; free_vars; generator; prems_rel; goal_rel },
            [prems_def; goal_def]))
  | None ->
    (* Generation mode: only filter premises, no goal. *)
    let prems = block.prems in
    let prems_with_decls =
      prepend_output_varpr_prems spec_il var_decls prems
    in
    (match Elaborate.elab_prems_in_spec spec_il var_decls prems_with_decls with
     | Error e ->
       Error (Elaborate.error_to_string e)
     | Ok (il_prems, output_vars) ->
       let input_ids = List.map (fun v -> v.Qc_ir.iv_id) free_vars in
       let input_pairs =
         List.map (fun v -> (v.Qc_ir.iv_id, v.Qc_ir.iv_typ)) free_vars
       in
       let prems_rel_id = Printf.sprintf "__qc_%s_prems__" name in
       let prems_all_vars = input_pairs @ output_vars in
       let prems_def =
         make_synth_rel_def prems_rel_id prems_all_vars
           (List.length free_vars) il_prems
       in
       let prems_rel = Qc_ir.{
         sr_id      = prems_rel_id;
         sr_inputs  = input_ids;
         sr_outputs = output_vars;
       } in
       Ok
         (Qc_ir.QcGen { name; free_vars; generator; prems_rel },
          [prems_def]))

(* --- top-level -------------------------------------------------------- *)

let elaborate (spec_il : spec) (ast : Qc_ast.ast_file) :
    (Qc_ir.t * spec, string) result =
  List.fold_right
    (fun block acc ->
      match acc with
      | Error _ -> acc
      | Ok (cmds, defs) ->
        (match elab_block spec_il block with
         | Error e -> Error e
         | Ok (cmd, new_defs) -> Ok (cmd :: cmds, defs @ new_defs)))
    ast (Ok ([], []))
