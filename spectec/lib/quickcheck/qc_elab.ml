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

(* --- block elaboration ------------------------------------------------ *)

let elab_block (spec_il : spec) (block : Qc_ast.ast_block) :
    (Qc_ir.qc_command, string) result =
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
    (match Elaborate.elab_prems_in_spec spec_il var_decls (prems @ [goal]) with
     | Error e -> Error (Elaborate.error_to_string e)
     | Ok il_all ->
       match List.rev il_all with
       | [] -> Error "quickcheck/prop: empty elaboration result"
       | il_goal :: il_prems_rev ->
         let il_prems = List.rev il_prems_rev in
         Ok
           (Qc_ir.QcProp
              { free_vars;
                goal = il_goal;
                prems = il_prems }))
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
     | Ok il_prems ->
       Ok (Qc_ir.QcGen
             { free_vars;
               prems = il_prems }))

(* --- top-level -------------------------------------------------------- *)

let elaborate (spec_il : spec) (ast : Qc_ast.ast_file) :
    (Qc_ir.t, string) result =
  List.fold_right
    (fun block acc ->
      match acc with
      | Error _ -> acc
      | Ok cmds ->
        (match elab_block spec_il block with
         | Error e -> Error e
         | Ok cmd -> Ok (cmd :: cmds)))
    ast (Ok [])
