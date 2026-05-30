open Common.Source
open Common.Domain
open Lang
open Lang.Il
module HEnv = Envs.HEnv
module TDEnv = Envs.Il.TDEnv

(* Structuring premises *)

let rec internalize_iter ?(iterexps : iterexp list = []) (prem : prem) :
    prem * iterexp list =
  match prem.it with
  | IterPr (prem, iterexp) ->
      internalize_iter ~iterexps:(iterexp :: iterexps) prem
  | _ -> (prem, iterexps)

let rec struct_prems (prems : prem list) (instr_ret : Ol.Ast.instr) :
    Ol.Ast.instr =
  let prems_internalized = List.map internalize_iter prems in
  struct_prems' prems_internalized instr_ret

and struct_prems' (prems_internalized : (prem * iterexp list) list)
    (instr_ret : Ol.Ast.instr) : Ol.Ast.instr =
  match prems_internalized with
  | [] -> instr_ret
  | [ ({ it = ElsePr; at; _ }, []) ] -> Ol.Ast.OtherwiseI instr_ret $ at
  | (prem_h, iterexps_h) :: prems_internalized_t -> (
      let at = prem_h.at in
      match prem_h.it with
      | RulePr { relid = id; notexp } ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.RuleI (id, notexp, iterexps_h, [ instr_t ]) $ at
      | IfPr exp ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.IfI (exp, iterexps_h, [ instr_t ]) $ at
      | IfHoldPr { relid = id; notexp } ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.IfHoldI (id, notexp, iterexps_h, [ instr_t ]) $ at
      | IfNotHoldPr { relid = id; notexp } ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.IfNotHoldI (id, notexp, iterexps_h, [ instr_t ]) $ at
      | LetPr (exp_l, exp_r) ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.LetI (exp_l, exp_r, iterexps_h, [ instr_t ]) $ at
      | DebugPr exp ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.DebugI (exp, instr_t) $ at
      | _ -> assert false)

let split_else_path ((prems, payload) : prem list * 'a) :
    (prem list * 'a) option * (prem list * 'a) =
  match List.rev prems with
  | { it = ElsePr; _ } :: prems_rev ->
      (Some (List.rev prems_rev, payload), (prems, payload))
  | _ -> (None, (prems, payload))

let partition_else_paths (paths : (prem list * 'a) list) :
    (prem list * 'a) list * (prem list * 'a) option =
  let normal_paths, else_paths =
    List.fold_right
      (fun path (normal_paths, else_paths) ->
        match split_else_path path with
        | Some else_path, _ ->
            if else_paths <> None then
              failwith "multiple otherwise paths are not supported"
            else (normal_paths, Some else_path)
        | None, normal_path -> (normal_path :: normal_paths, else_paths))
      paths ([], None)
  in
  (normal_paths, else_paths)

(* Structuring rules *)

let struct_rule_path ((prems, exps_output) : prem list * exp list) :
    Ol.Ast.instr list =
  let at =
    (* if exps_output is empty, use last prem's region *)
    if exps_output = [] then
      match List.rev prems with
      | prem_last :: _ -> prem_last.at
      | [] -> no_region
    else exps_output |> List.map at |> over_region
  in
  let instr_ret = Ol.Ast.ResultI exps_output $ at in
  let prems =
    match List.rev prems with
    | { it = ElsePr; _ } :: prems_rev -> List.rev prems_rev
    | _ -> prems
  in
  [ struct_prems prems instr_ret ]

(* Structuring clauses *)

let struct_clause_path ((prems, exp_output) : prem list * exp) :
    Ol.Ast.instr list =
  let at = exp_output.at in
  let instr_ret = Ol.Ast.ReturnI exp_output $ at in
  let prems =
    match List.rev prems with
    | { it = ElsePr; _ } :: prems_rev -> List.rev prems_rev
    | _ -> prems
  in
  [ struct_prems prems instr_ret ]

(* Structuring definitions *)

let rec struct_def (henv : HEnv.t) (tdenv : TDEnv.t) (def : def) : Sl.def =
  let at = def.at in
  match def.it with
  | TypD { synid = id; tparams; deftyp } -> Sl.TypD (id, tparams, deftyp) $ at
  | RelD { relid = id; notation; inputs; rules } ->
      struct_rel_def henv tdenv at id notation inputs rules
  | BuiltinDecD { defid = id; tparams; params; _ } ->
      struct_builtin_dec_def at id tparams params
  | DecD { defid = id; tparams; clauses; _ } ->
      struct_dec_def henv tdenv at id tparams clauses

(* Structuring relation definitions *)

and struct_rel_def (henv : HEnv.t) (tdenv : TDEnv.t) (at : region) (id_rel : id)
    (nottyp : nottyp) (inputs : int list) (rules : rule list) : Sl.def =
  let mixop = Il.Mixfix.to_mixop nottyp.it in
  let exps_input, paths = Antiunify.antiunify_rules inputs rules in
  let block_paths, else_path_opt = partition_else_paths paths in
  let block =
    List.concat_map struct_rule_path block_paths |> Optimize.optimize henv tdenv
  in
  let elseblock_opt =
    Option.map
      (fun path -> struct_rule_path path |> Optimize.optimize henv tdenv)
      else_path_opt
  in
  let block, elseblock_opt = Instrument.instrument tdenv block elseblock_opt in
  Sl.RelD (id_rel, (mixop, inputs), exps_input, block, elseblock_opt) $ at

(* Structuring builtin declaration definitions *)

and struct_builtin_dec_def (at : region) (id_dec : id) (tparams : tparam list)
    (params : param list) : Sl.def =
  let args_input, _ =
    List.fold_left
      (fun (args_input, frees) param ->
        let arg_input, frees =
          match param.it with
          | ExpP typ ->
              let exp_input, frees =
                Elaborate.Fresh.fresh_exp_from_typ frees typ
              in
              let arg_input = ExpA exp_input $ param.at in
              (arg_input, frees)
          | DefP { defid = id_def; _ } ->
              let arg_input = DefA id_def $ param.at in
              (arg_input, frees)
        in
        (args_input @ [ arg_input ], frees))
      ([], IdSet.empty) params
  in
  Sl.BuiltinDecD (id_dec, tparams, args_input) $ at

(* Structuring declaration definitions *)

and struct_dec_def (henv : HEnv.t) (tdenv : TDEnv.t) (at : region) (id_dec : id)
    (tparams : tparam list) (clauses : clause list) : Sl.def =
  let args_input, paths = Antiunify.antiunify_clauses clauses in
  let block_paths, else_path_opt = partition_else_paths paths in
  let block =
    List.concat_map struct_clause_path block_paths
    |> Optimize.optimize henv tdenv
  in
  let elseblock_opt =
    Option.map
      (fun path -> struct_clause_path path |> Optimize.optimize henv tdenv)
      else_path_opt
  in
  let block, elseblock_opt = Instrument.instrument tdenv block elseblock_opt in
  Sl.DecD (id_dec, tparams, args_input, block, elseblock_opt) $ at

(* Load type definitions *)

let load_def (henv : HEnv.t) (tdenv : TDEnv.t) (def : def) : HEnv.t * TDEnv.t =
  match def.it with
  | TypD { synid = id; tparams; deftyp } ->
      let typdef = (tparams, deftyp) in
      let tdenv = TDEnv.add id typdef tdenv in
      (henv, tdenv)
  | RelD { relid = id; inputs; _ } ->
      let henv = HEnv.add id inputs henv in
      (henv, tdenv)
  | _ -> (henv, tdenv)

let load_spec (henv : HEnv.t) (tdenv : TDEnv.t) (spec : spec) : HEnv.t * TDEnv.t
    =
  List.fold_left
    (fun (henv, tdenv) def -> load_def henv tdenv def)
    (henv, tdenv) spec

(* Structuring a spec *)

let struct_spec (spec : spec) : Sl.spec =
  let henv, tdenv = load_spec HEnv.empty TDEnv.empty spec in
  List.map (struct_def henv tdenv) spec
