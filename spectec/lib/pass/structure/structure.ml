open Common.Annotated
module Location = Common.Location
open Common.Domain
open Lang
open Lang.Il.Expression
open Lang.Il
open Lang.Al
module RTEnv = Envs.RTEnv
module SEnv = Envs.Il.SEnv

(* Structuring instructions *)

let rec internalize_iter ?(iterinstrs : iterinstr list = []) (instr : instr) :
    instr * iterinstr list =
  match instr.it with
  | IterI (instr, iterinstr) ->
      internalize_iter ~iterinstrs:(iterinstr :: iterinstrs) instr
  | _ -> (instr, iterinstrs)

let condition_iterexps (iterinstrs : iterinstr list) : iterexp list =
  List.map
    (fun (iterinstr : iterinstr) ->
      assert (iterinstr.vars_output = []);
      iterexp_of_iterinstr iterinstr)
    iterinstrs

let rec struct_instrs (instrs : instr list) (instr_ret : Ol.Ast.instr) :
    Ol.Ast.instr =
  let instrs_internalized = List.map internalize_iter instrs in
  struct_instrs' instrs_internalized instr_ret

and struct_instrs' (instrs_internalized : (instr * iterinstr list) list)
    (instr_ret : Ol.Ast.instr) : Ol.Ast.instr =
  match instrs_internalized with
  | [] -> instr_ret
  | (instr_h, iterinstrs_h) :: instrs_internalized_t -> (
      let at = instr_h.at in
      match instr_h.it with
      | RelI { relid = id; notexp } ->
          let instr_rest = struct_instrs' instrs_internalized_t instr_ret in
          let call = Ol.Ast.{ relid = id; notexp } in
          Ol.Ast.RelI
            { call; iterinstrs = iterinstrs_h; block = [ instr_rest ] }
          $ at
      | IfI { cond; _ } ->
          let instr_rest = struct_instrs' instrs_internalized_t instr_ret in
          Ol.Ast.IfI (cond, condition_iterexps iterinstrs_h, [ instr_rest ])
          $ at
      | RelAssertI { call = { relid = id; notexp }; expect } ->
          let instr_rest = struct_instrs' instrs_internalized_t instr_ret in
          let call = Ol.Ast.{ relid = id; notexp } in
          Ol.Ast.RelAssertI
            {
              call;
              expect;
              iterexps = condition_iterexps iterinstrs_h;
              block = [ instr_rest ];
            }
          $ at
      | LetI (exp_l, exp_r) ->
          let instr_rest = struct_instrs' instrs_internalized_t instr_ret in
          Ol.Ast.LetI (exp_l, exp_r, iterinstrs_h, [ instr_rest ]) $ at
      | DebugI exp ->
          let instr_rest = struct_instrs' instrs_internalized_t instr_ret in
          Ol.Ast.DebugI (exp, instr_rest) $ at
      | _ -> assert false)

let split_otherwise_path ((instrs, payload) : instr list * 'a) :
    (instr list * 'a) option * (instr list * 'a) =
  match List.rev instrs with
  | { it = OtherwiseI; _ } :: instrs_rev ->
      (Some (List.rev instrs_rev, payload), (instrs, payload))
  | _ -> (None, (instrs, payload))

let partition_otherwise_paths (paths : (instr list * 'a) list) :
    (instr list * 'a) list * (instr list * 'a) option =
  let normal_paths, otherwise_paths =
    List.fold_right
      (fun path (normal_paths, otherwise_paths) ->
        match split_otherwise_path path with
        | Some otherwise_path, _ ->
            if otherwise_paths <> None then
              failwith "multiple otherwise paths are not supported"
            else (normal_paths, Some otherwise_path)
        | None, normal_path -> (normal_path :: normal_paths, otherwise_paths))
      paths ([], None)
  in
  (normal_paths, otherwise_paths)

(* Structuring rules *)

let struct_rule_path ((instrs, exps_output) : instr list * exp list) :
    Ol.Ast.instr list =
  let at =
    if exps_output = [] then
      match List.rev instrs with
      | instr_last :: _ -> instr_last.at
      | [] -> Location.Unknown
    else exps_output |> List.map at |> Location.cover
  in
  let instr_ret = Ol.Ast.ResultI exps_output $ at in
  [ struct_instrs instrs instr_ret ]

(* Structuring clauses *)

let struct_clause_path ((instrs, exp_output) : instr list * exp) :
    Ol.Ast.instr list =
  let at = exp_output.at in
  let instr_ret = Ol.Ast.ReturnI exp_output $ at in
  [ struct_instrs instrs instr_ret ]

(* Structuring definitions *)

let args_input_of_params (params : param list) : arg list =
  List.fold_left
    (fun (args_input, frees) param ->
      let arg_input, frees =
        match param.it with
        | ExpP typ ->
            let exp_input, frees = Il.Fresh.fresh_exp_from_typ frees typ in
            (ExpA exp_input $ param.at, frees)
        | DefP { defid = id_def; _ } -> (DefA id_def $ param.at, frees)
      in
      (args_input @ [ arg_input ], frees))
    ([], IdSet.empty) params
  |> fst

let rec struct_def (rtenv : RTEnv.t) (senv : SEnv.t) (def : def) : Sl.def =
  let at = def.at in
  match def.it with
  | TypD { synid = id; tparams; deftyp } -> Sl.TypD (id, tparams, deftyp) $ at
  | RelD { relid = id; reltyp; rules } ->
      struct_rel_def rtenv senv at id reltyp rules
  | BuiltinDecD { defid = id; tparams; params; typ; _ } ->
      struct_builtin_dec_def at id tparams params typ
  | DecD { defid = id; tparams; params; clauses; _ } ->
      struct_dec_def rtenv senv at id tparams params clauses

(* Structuring relation definitions *)

and struct_rel_def (rtenv : RTEnv.t) (senv : SEnv.t) (at : Location.t)
    (id_rel : id) (reltyp : reltyp) (rules : rule list) : Sl.def =
  let exps_input, paths = Antiunify.antiunify_rules reltyp.it rules in
  let block_paths, otherwise_path_opt = partition_otherwise_paths paths in
  let block =
    List.concat_map struct_rule_path block_paths |> Optimize.optimize rtenv senv
  in
  let elseblock_opt =
    Option.map
      (fun path -> struct_rule_path path |> Optimize.optimize rtenv senv)
      otherwise_path_opt
  in
  let block, elseblock_opt = Instrument.instrument block elseblock_opt in
  let in_typs = Il.Mode.inputs reltyp.it in
  let exps_input =
    match rules with
    | [] ->
        (* The relation is never invoked, but the SL shape still needs one
           placeholder per input slot. *)
        let _, exps =
          List.fold_left
            (fun (frees, exps) typ ->
              let exp, frees = Il.Fresh.fresh_exp_from_typ frees typ in
              (frees, exps @ [ exp ]))
            (IdSet.empty, []) in_typs
        in
        exps
    | _ ->
        assert (List.length exps_input = List.length in_typs);
        exps_input
  in
  let sl_mode = Il.Mode.with_inputs reltyp.it exps_input in
  Sl.RelD (id_rel, sl_mode, block, elseblock_opt) $ at

(* Structuring builtin declaration definitions *)

and struct_builtin_dec_def (at : Location.t) (id_dec : id)
    (tparams : tparam list) (params : param list) (typ : typ) : Sl.def =
  Sl.BuiltinDecD (id_dec, tparams, args_input_of_params params, typ) $ at

(* Structuring declaration definitions *)

and struct_dec_def (rtenv : RTEnv.t) (senv : SEnv.t) (at : Location.t)
    (id_dec : id) (tparams : tparam list) (params : param list)
    (clauses : clause list) : Sl.def =
  let args_input, paths =
    match clauses with
    | [] -> (args_input_of_params params, [])
    | _ -> Antiunify.antiunify_clauses clauses
  in
  let block_paths, otherwise_path_opt = partition_otherwise_paths paths in
  let block =
    List.concat_map struct_clause_path block_paths
    |> Optimize.optimize rtenv senv
  in
  let elseblock_opt =
    Option.map
      (fun path -> struct_clause_path path |> Optimize.optimize rtenv senv)
      otherwise_path_opt
  in
  let block, elseblock_opt = Instrument.instrument block elseblock_opt in
  Sl.DecD (id_dec, tparams, args_input, block, elseblock_opt) $ at

let load_def (rtenv : RTEnv.t) (senv : SEnv.t) (def : def) : RTEnv.t * SEnv.t =
  match def.it with
  | TypD { synid = id; tparams; deftyp } ->
      let syntax = (tparams, deftyp) in
      let senv = SEnv.add id syntax senv in
      (rtenv, senv)
  | RelD { relid = id; reltyp; _ } ->
      let rtenv = RTEnv.add id reltyp rtenv in
      (rtenv, senv)
  | _ -> (rtenv, senv)

let load_spec (rtenv : RTEnv.t) (senv : SEnv.t) (spec : spec) : RTEnv.t * SEnv.t
    =
  List.fold_left
    (fun (rtenv, senv) def -> load_def rtenv senv def)
    (rtenv, senv) spec

(* Structuring a spec *)

let struct_spec (spec : spec) : Sl.spec =
  let rtenv, senv = load_spec RTEnv.empty SEnv.empty spec in
  List.map (struct_def rtenv senv) spec
