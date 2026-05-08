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

let block_counter = ref 0

(* --- block elaboration ------------------------------------------------ *)

let elab_block (spec_il : spec) (block : Qc_ast.ast_block) :
    (Qc_ir.qc_command * def list, string) result =
  let n = !block_counter in
  incr block_counter;
  match block with
  | Qc_ast.AB_Prop { params; goal; prems } ->
    let free_vars =
      List.map
        (fun p ->
          { Qc_ir.iv_id = p.Qc_ast.p_id.it;
            iv_typ = elab_plaintyp p.Qc_ast.p_typ })
        params
    in
    let var_decls =
      List.map (fun p -> (p.Qc_ast.p_id, p.Qc_ast.p_typ)) params
    in
    (* Elaborate prems alone to get only prems-bound output vars.
       The goal may introduce locals (e.g. wildcards → _0) that must NOT
       appear as outputs of the prems relation. *)
    (match Elaborate.elab_prems_in_spec spec_il var_decls prems with
     | Error e -> Error (Elaborate.error_to_string e)
     | Ok (il_prems, prems_output_vars) ->
       (* Elaborate prems @ [goal] together so the goal sees prems-bound vars. *)
       match Elaborate.elab_prems_in_spec spec_il var_decls (prems @ [goal]) with
       | Error e -> Error (Elaborate.error_to_string e)
       | Ok (il_all, _) ->
         let il_goal =
           match List.nth_opt il_all (List.length il_prems) with
           | None -> failwith "quickcheck/prop: goal not found after elaboration"
           | Some g -> g
         in
         let input_ids = List.map (fun v -> v.Qc_ir.iv_id) free_vars in
         let input_pairs =
           List.map (fun v -> (v.Qc_ir.iv_id, v.Qc_ir.iv_typ)) free_vars
         in
         (* prems relation: inputs = free_vars, outputs = prems-bound vars only *)
         let prems_rel_id = Printf.sprintf "__qc_%d_prems__" n in
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
         (* goal relation: all known vars as inputs, no outputs.
            Goal-local vars (wildcards etc.) are internal to the rule body. *)
         let goal_rel_id = Printf.sprintf "__qc_%d_goal__" n in
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
           (Qc_ir.QcProp { free_vars; prems_rel; goal_rel },
            [prems_def; goal_def]))
  | Qc_ast.AB_Gen { params; prems } ->
    let free_vars =
      List.map
        (fun p ->
          { Qc_ir.iv_id = p.Qc_ast.p_id.it;
            iv_typ = elab_plaintyp p.Qc_ast.p_typ })
        params
    in
    let var_decls =
      List.map (fun p -> (p.Qc_ast.p_id, p.Qc_ast.p_typ)) params
    in
    (match Elaborate.elab_prems_in_spec spec_il var_decls prems with
     | Error e ->
       Error (Elaborate.error_to_string e)
     | Ok (il_prems, output_vars) ->
       let input_ids = List.map (fun v -> v.Qc_ir.iv_id) free_vars in
       let input_pairs =
         List.map (fun v -> (v.Qc_ir.iv_id, v.Qc_ir.iv_typ)) free_vars
       in
       let prems_rel_id = Printf.sprintf "__qc_%d_prems__" n in
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
         (Qc_ir.QcGen { free_vars; prems_rel },
          [prems_def]))

(* --- top-level -------------------------------------------------------- *)

let elaborate (spec_il : spec) (ast : Qc_ast.ast_file) :
    (Qc_ir.t * spec, string) result =
  block_counter := 0;
  List.fold_right
    (fun block acc ->
      match acc with
      | Error _ -> acc
      | Ok (cmds, defs) ->
        (match elab_block spec_il block with
         | Error e -> Error e
         | Ok (cmd, new_defs) -> Ok (cmd :: cmds, defs @ new_defs)))
    ast (Ok ([], []))
