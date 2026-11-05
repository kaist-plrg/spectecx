open Common.Annotated
open Common.Domain
open Lang
open Lang.Xl
open Il.Expression
open Ol.Ast
module RTEnv = Envs.RTEnv
module SEnv = Envs.Il.SEnv

(* The algo pass ensures that each variable is bound at most once per rule.
   Before [optimize_loop] combines rules, the renamer can therefore replace
   [y] with [x] throughout [let y = x]'s block without checking for a nested
   binding. *)
let remove_let_alias (instrs : instr list) : instr list =
  Ol.Traverse.rewrite_siblings_preorder instrs ~f:(fun instr_h instrs_t ->
      match instr_h.it with
      | LetI ({ it = VarE id_l; _ }, { it = VarE id_r; _ }, _, block) ->
          let rename = Renamer.Rename.singleton id_l id_r in
          Some (Renamer.rename_instrs rename block @ instrs_t)
      | _ -> None)

(* Matchify equals terminal *)

let matchify_exp_eq_terminal (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | CmpE (`EqOp, _, exp_l, { it = CaseE notexp; _ })
    when Il.Mixfix.args notexp = [] ->
      MatchE (exp_l, CaseP (Il.Mixfix.to_mixop notexp)) $$ (at, note)
  | CmpE (`NeOp, _, exp_l, { it = CaseE notexp; _ })
    when Il.Mixfix.args notexp = [] ->
      let exp =
        MatchE (exp_l, CaseP (Il.Mixfix.to_mixop notexp)) $$ (at, note)
      in
      UnE (`NotOp, `BoolT, exp) $$ (at, note)
  | _ -> exp

let matchify_if_eq_terminals (instrs : instr list) : instr list =
  Ol.Traverse.rewrite_instrs_postorder instrs ~f:(fun instr ->
      match instr.it with
      | IfI (exp_cond, iterexps, instrs_then) ->
          let exp_cond = matchify_exp_eq_terminal exp_cond in
          { instr with it = IfI (exp_cond, iterexps, instrs_then) }
      | _ -> instr)

(* Let and relation bindings *)

module Bind = struct
  (* Expression unit *)

  type expunit = exp * iterinstr list

  let eq_expunit (expunit_a : expunit) (expunit_b : expunit) : bool =
    let exp_a, iterinstrs_a = expunit_a in
    let exp_b, iterinstrs_b = expunit_b in
    Sl.Eq.eq_exp exp_a exp_b && Sl.Eq.eq_iterinstrs iterinstrs_a iterinstrs_b

  let eq_expunits (expunits_a : expunit list) (expunits_b : expunit list) : bool
      =
    List.length expunits_a = List.length expunits_b
    && List.for_all2 eq_expunit expunits_a expunits_b

  (* Binding *)

  type t =
    | LetBind of expunit * expunit
    | RelBind of id * expunit list * expunit list

  (* Constructors *)

  let init_expunit (exp : exp) (iterinstrs : iterinstr list) : expunit =
    let ids = Il.Free.free_exp exp in
    let iterinstrs =
      List.map
        (fun (iterinstr : iterinstr) ->
          let filter_vars vars =
            List.filter (fun var -> IdSet.mem var.Il.varid ids) vars
          in
          {
            iterinstr with
            vars_input = filter_vars iterinstr.vars_input;
            vars_output = filter_vars iterinstr.vars_output;
          })
        iterinstrs
    in
    (exp, iterinstrs)

  let init_let_bind (exp_l : exp) (exp_r : exp) (iterinstrs : iterinstr list) :
      t =
    let expunit_l = init_expunit exp_l iterinstrs in
    let expunit_r = init_expunit exp_r iterinstrs in
    LetBind (expunit_l, expunit_r)

  let init_rel_bind (rtenv : RTEnv.t) (id : id) (notexp : notexp)
      (iterinstrs : iterinstr list) : t =
    let exps_l, exps_r =
      let exps = Il.Mixfix.args notexp in
      let reltyp = RTEnv.find id rtenv in
      Il.Mode.partition reltyp.it exps
    in
    let expunits_l =
      List.map (fun exp_l -> init_expunit exp_l iterinstrs) exps_l
    in
    let expunits_r =
      List.map (fun exp_r -> init_expunit exp_r iterinstrs) exps_r
    in
    RelBind (id, expunits_l, expunits_r)

  (* Collapsing two bindings,
     if two bindings have syntactically equal right-hand sides,
     and the left-hand sides are equal up to renaming,
     then we can collapse them into a single binding *)

  let rec collapse_exp (rename : Renamer.t) (exp : exp) (exp_target : exp) :
      Renamer.t option =
    match (exp.it, exp_target.it) with
    | VarE id, VarE id_target ->
        let rename =
          if Sl.Eq.eq_id id id_target then rename
          else Renamer.Rename.add id_target id rename
        in
        Some rename
    | TupleE exps, TupleE exps_target -> collapse_exps rename exps exps_target
    | CaseE notexp, CaseE notexp_target
      when Il.Mixfix.eq_mixop notexp notexp_target ->
        collapse_exps rename (Il.Mixfix.args notexp)
          (Il.Mixfix.args notexp_target)
    | StrE expfields, StrE expfields_target ->
        let atoms, exps = List.split expfields in
        let atoms_target, exps_target = List.split expfields_target in
        if Sl.Eq.eq_atoms atoms atoms_target then
          collapse_exps rename exps exps_target
        else None
    | OptE exp_opt, OptE exp_opt_target -> (
        match (exp_opt, exp_opt_target) with
        | Some exp, Some exp_target -> collapse_exp rename exp exp_target
        | None, None -> Some rename
        | _ -> None)
    | ListE exps, ListE exps_target -> collapse_exps rename exps exps_target
    | ConsE (exp_h, exp_t), ConsE (exp_h_target, exp_t_target) ->
        collapse_exps rename [ exp_h; exp_t ] [ exp_h_target; exp_t_target ]
    | IterE (exp, iterexp), IterE (exp_target, iterexp_target) -> (
        match collapse_exp rename exp exp_target with
        | Some rename ->
            let iterexp_target_renamed =
              Renamer.rename_iterexp rename iterexp_target
            in
            if Sl.Eq.eq_iterexp iterexp iterexp_target_renamed then Some rename
            else None
        | None -> None)
    | _ -> None

  and collapse_exps (rename : Renamer.t) (exps : exp list)
      (exps_target : exp list) : Renamer.t option =
    match (exps, exps_target) with
    | [], [] -> Some rename
    | exp_h :: exps_t, exp_target_h :: exps_target_t -> (
        match collapse_exp rename exp_h exp_target_h with
        | Some rename -> collapse_exps rename exps_t exps_target_t
        | None -> None)
    | _ -> None

  let collapse_expunit (rename : Renamer.t) (expunit : expunit)
      (expunit_target : expunit) : Renamer.t option =
    let exp, iterinstrs = expunit in
    let exp_target, iterinstrs_target = expunit_target in
    let rename_opt = collapse_exp rename exp exp_target in
    match rename_opt with
    | Some rename ->
        let iterinstrs_target_renamed =
          Renamer.rename_iterinstrs rename iterinstrs_target
        in
        if Sl.Eq.eq_iterinstrs iterinstrs iterinstrs_target_renamed then
          Some rename
        else None
    | None -> None

  let rec collapse_expunits (rename : Renamer.t) (expunits : expunit list)
      (expunits_target : expunit list) : Renamer.t option =
    match (expunits, expunits_target) with
    | [], [] -> Some rename
    | expunit_h :: expunits_t, expunit_target_h :: expunits_target_t -> (
        match collapse_expunit rename expunit_h expunit_target_h with
        | Some rename -> collapse_expunits rename expunits_t expunits_target_t
        | None -> None)
    | _ -> None

  let collapse_bind (bind : t) (bind_target : t) : Renamer.t option =
    match (bind, bind_target) with
    | ( LetBind (expunit_l, expunit_r),
        LetBind (expunit_target_l, expunit_target_r) )
      when eq_expunit expunit_r expunit_target_r ->
        collapse_expunit Renamer.Rename.empty expunit_l expunit_target_l
    | ( RelBind (id, expunits_l, expunits_r),
        RelBind (id_target, expunits_target_l, expunits_target_r) )
      when Sl.Eq.eq_id id id_target && eq_expunits expunits_r expunits_target_r
      ->
        collapse_expunits Renamer.Rename.empty expunits_l expunits_target_l
    | _ -> None
end

(* Block and binding merges *)

let rec merge_block (instrs_a : instr list) (instrs_b : instr list) : instr list
    =
  match (instrs_a, instrs_b) with
  | ( { it = IfI (exp_cond_a, iterexps_a, instrs_then_a); at; _ } :: instrs_a,
      { it = IfI (exp_cond_b, iterexps_b, instrs_then_b); _ } :: instrs_b )
    when Sl.Eq.eq_exp exp_cond_a exp_cond_b
         && Sl.Eq.eq_iterexps iterexps_a iterexps_b ->
      let instrs_then = merge_block instrs_then_a instrs_then_b in
      let instr_h = IfI (exp_cond_a, iterexps_a, instrs_then) $ at in
      merge_block (instr_h :: instrs_a) instrs_b
  | _ -> instrs_a @ instrs_b

let merge_into_block (instr : instr) ~(later : instr list) : instr =
  match instr.it with
  | RelI _ | RelAssertI _ | IfI _ | LetI _ ->
      Ol.Traverse.map_instr_blocks instr ~f:(fun block ->
          merge_block block later)
  | CaseI _ | ResultI _ | ReturnI _ | DebugI _ -> assert false

let as_binding_with_block_opt (instr : instr) ~(rtenv : RTEnv.t) :
    (Bind.t * instr list) option =
  match instr.it with
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      Some (Bind.init_let_bind exp_l exp_r iterinstrs, block)
  | RelI { call; iterinstrs; block } ->
      Some (Bind.init_rel_bind rtenv call.relid call.notexp iterinstrs, block)
  | _ -> None

let merge_binding (rtenv : RTEnv.t) (instrs : instr list) : instr list =
  Ol.Traverse.merge_siblings_preorder instrs
    ~f:(fun instr_earlier instr_later ->
      match
        ( as_binding_with_block_opt instr_earlier ~rtenv,
          as_binding_with_block_opt instr_later ~rtenv )
      with
      | Some (bind_earlier, _), Some (bind_later, block_later) -> (
          match Bind.collapse_bind bind_earlier bind_later with
          | Some rename ->
              Take
                (merge_into_block instr_earlier
                   ~later:(Renamer.rename_instrs rename block_later))
          | None -> Stop)
      | _ -> Stop)

(* Syntactic analysis of conditions

   Note that this is best-effort analysis,
   since even semantic analysis cannot guarantee completeness of the analysis *)

(* Conversion between guard and conditional expression *)

let exp_as_guard (exp_target : exp) (exp_cond : exp) : guard option =
  match exp_cond.it with
  | UnE (`NotOp, _, exp) when Sl.Eq.eq_exp exp_target exp -> Some (BoolG false)
  | CmpE (`EqOp, optyp, exp_l, exp_r) when Sl.Eq.eq_exp exp_target exp_l ->
      Some (CmpG (`EqOp, optyp, exp_r))
  | CmpE (`EqOp, optyp, exp_l, exp_r) when Sl.Eq.eq_exp exp_target exp_r ->
      Some (CmpG (`EqOp, optyp, exp_l))
  | CmpE (`NeOp, optyp, exp_l, exp_r) when Sl.Eq.eq_exp exp_target exp_l ->
      Some (CmpG (`NeOp, optyp, exp_r))
  | CmpE (`NeOp, optyp, exp_l, exp_r) when Sl.Eq.eq_exp exp_target exp_r ->
      Some (CmpG (`NeOp, optyp, exp_l))
  | SubE (exp, typ) when Sl.Eq.eq_exp exp_target exp -> Some (SubG typ)
  | MatchE (exp, pattern) when Sl.Eq.eq_exp exp_target exp ->
      Some (MatchG pattern)
  | MemE (exp_e, exp_s) when Sl.Eq.eq_exp exp_target exp_e -> Some (MemG exp_s)
  | _ -> None

let guard_as_exp (exp_target : exp) (guard : guard) : exp =
  match guard with
  | BoolG true -> exp_target
  | BoolG false ->
      UnE (`NotOp, `BoolT, exp_target)
      $$ (exp_target.at, { typ = Il.BoolT; annot = () })
  | CmpG (cmpop, optyp, exp) ->
      CmpE (cmpop, optyp, exp_target, exp)
      $$ (exp_target.at, { typ = Il.BoolT; annot = () })
  | SubG typ ->
      SubE (exp_target, typ) $$ (exp_target.at, { typ = Il.BoolT; annot = () })
  | MatchG pattern ->
      MatchE (exp_target, pattern)
      $$ (exp_target.at, { typ = Il.BoolT; annot = () })
  | MemG exp ->
      MemE (exp_target, exp) $$ (exp_target.at, { typ = Il.BoolT; annot = () })

(* Conversion from type to its variants *)

let rec typ_as_variant (senv : SEnv.t) (typ : typ) : mixop list option =
  match typ.it with
  | VarT { synid = tid; _ } -> (
      let _, deftyp = SEnv.find tid senv in
      match deftyp.it with
      | PlainT typ -> typ_as_variant senv typ
      | VariantT typcases ->
          let mixops =
            typcases
            |> List.map (fun { Il.notation; _ } ->
                   Il.Mixfix.to_mixop notation.it)
          in
          Some mixops
      | _ -> None)
  | _ -> None

(* Determine the overlapping guard of two conditions *)

type overlap =
  | Identical
  | Disjoint of exp * guard * guard
  | Partition of exp * guard * guard
  | Fuzzy

let rec distinct_exp_literal (exp_a : exp) (exp_b : exp) : bool =
  match (exp_a.it, exp_b.it) with
  | BoolE b_a, BoolE b_b -> b_a <> b_b
  | NumE n_a, NumE n_b -> not (Num.eq n_a n_b)
  | TextE t_a, TextE t_b -> t_a <> t_b
  | TupleE exps_a, TupleE exps_b ->
      assert (List.length exps_a = List.length exps_b);
      List.exists2 distinct_exp_literal exps_a exps_b
  | CaseE notexp_a, CaseE notexp_b
    when Il.Mixfix.args notexp_a = [] && Il.Mixfix.args notexp_b = [] ->
      not (Il.Mixfix.eq_mixop notexp_a notexp_b)
  | ListE exps_a, ListE exps_b when List.length exps_a = List.length exps_b ->
      List.exists2 distinct_exp_literal exps_a exps_b
  | ListE _, ListE _ -> true
  | _ -> false

let overlap_typ (senv : SEnv.t) (exp : exp) (typ_a : typ) (typ_b : typ) :
    overlap =
  let guard_a = SubG typ_a in
  let guard_b = SubG typ_b in
  match (typ_as_variant senv typ_a, typ_as_variant senv typ_b) with
  | Some mixops_a, Some mixops_b ->
      let module Set = Set.Make (struct
        type t = Il.Mixfix.mixop

        let compare = Il.Mixfix.compare_mixop
      end) in
      let mixops_a = Set.of_list mixops_a in
      let mixops_b = Set.of_list mixops_b in
      if Set.equal mixops_a mixops_b then Identical
      else if Set.inter mixops_a mixops_b |> Set.is_empty then
        Disjoint (exp, guard_a, guard_b)
      else Fuzzy
  | _ -> Fuzzy

let rec overlap_exp (senv : SEnv.t) (exp_a : exp) (exp_b : exp) : overlap =
  let overlap_exp_unequal () : overlap =
    match (exp_a.it, exp_b.it) with
    (* Negation *)
    | UnE (`NotOp, _, exp_a), _ when Sl.Eq.eq_exp exp_a exp_b ->
        Partition (exp_a, BoolG false, BoolG true)
    | _, UnE (`NotOp, _, exp_b) when Sl.Eq.eq_exp exp_a exp_b ->
        Partition (exp_b, BoolG true, BoolG false)
    (* Equals literal *)
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b
           && Sl.Eq.eq_exp exp_a_l exp_b_l
           && distinct_exp_literal exp_a_r exp_b_r ->
        Disjoint
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`EqOp, optyp_b, exp_b_r) )
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b
           && Sl.Eq.eq_exp exp_a_l exp_b_r
           && distinct_exp_literal exp_a_r exp_b_l ->
        Disjoint
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`EqOp, optyp_b, exp_b_l) )
    (* Equals and not equals *)
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`NeOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b
           && Sl.Eq.eq_exp exp_a_l exp_b_l
           && Sl.Eq.eq_exp exp_a_r exp_b_r ->
        Partition
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`NeOp, optyp_b, exp_b_r) )
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`NeOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b
           && Sl.Eq.eq_exp exp_a_l exp_b_r
           && Sl.Eq.eq_exp exp_a_r exp_b_l ->
        Partition
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`NeOp, optyp_b, exp_b_l) )
    (* Subtyping *)
    | SubE (exp_a, typ_a), SubE (exp_b, typ_b) when Sl.Eq.eq_exp exp_a exp_b ->
        overlap_typ senv exp_a typ_a typ_b
    (* Match on patterns *)
    | MatchE (exp_a, pattern_a), MatchE (exp_b, pattern_b)
      when Sl.Eq.eq_exp exp_a exp_b ->
        overlap_pattern exp_a pattern_a pattern_b
    (* Membership on literals *)
    | ( MemE (exp_e_a, ({ it = ListE exps_s_a; _ } as exp_s_a)),
        MemE (exp_e_b, ({ it = ListE exps_s_b; _ } as exp_s_b)) )
      when Sl.Eq.eq_exp exp_e_a exp_e_b
           && List.for_all
                (fun exp_s_a ->
                  List.for_all (distinct_exp_literal exp_s_a) exps_s_b)
                exps_s_a ->
        Disjoint (exp_e_a, MemG exp_s_a, MemG exp_s_b)
    | _ -> Fuzzy
  in
  if Sl.Eq.eq_exp exp_a exp_b then Identical else overlap_exp_unequal ()

and overlap_pattern (exp : exp) (pattern_a : pattern) (pattern_b : pattern) :
    overlap =
  let guard_a = MatchG pattern_a in
  let guard_b = MatchG pattern_b in
  let overlap_pattern_unequal () : overlap =
    match (pattern_a, pattern_b) with
    | CaseP _, CaseP _ -> Disjoint (exp, guard_a, guard_b)
    | ListP `Cons, ListP (`Fixed n) | ListP (`Fixed n), ListP `Cons ->
        if n = 0 then Partition (exp, guard_a, guard_b) else Fuzzy
    | ListP `Cons, ListP `Nil | ListP `Nil, ListP `Cons ->
        Partition (exp, guard_a, guard_b)
    | ListP (`Fixed _), ListP (`Fixed _) -> Disjoint (exp, guard_a, guard_b)
    | ListP (`Fixed n), ListP `Nil | ListP `Nil, ListP (`Fixed n) ->
        if n = 0 then Identical else Disjoint (exp, guard_a, guard_b)
    | OptP `Some, OptP `None | OptP `None, OptP `Some ->
        Partition (exp, guard_a, guard_b)
    | _ -> Fuzzy
  in
  if Sl.Eq.eq_pattern pattern_a pattern_b then Identical
  else overlap_pattern_unequal ()

let overlap_guard (senv : SEnv.t) (exp : exp) (guard_a : guard)
    (guard_b : guard) : overlap =
  let exp_a = guard_as_exp exp guard_a in
  let exp_b = guard_as_exp exp guard_b in
  overlap_exp senv exp_a exp_b

(* Merge sibling if statements with the same condition *)

let merge_if (senv : SEnv.t) (instrs : instr list) : instr list =
  Ol.Traverse.merge_siblings_preorder instrs
    ~f:(fun instr_earlier instr_later ->
      match (instr_earlier.it, instr_later.it) with
      | ( IfI (exp_cond_earlier, iterexps_earlier, _),
          IfI (exp_cond_later, iterexps_later, instrs_then_later) ) -> (
          let eq_iterexps = Sl.Eq.eq_iterexps iterexps_later iterexps_earlier in
          match
            (eq_iterexps, overlap_exp senv exp_cond_earlier exp_cond_later)
          with
          | true, Identical ->
              Take (merge_into_block instr_earlier ~later:instrs_then_later)
          | _ -> Skip)
      | _ -> Stop)

(* Merge sibling relation assertions with the same call *)

let merge_rel_assert (instrs : instr list) : instr list =
  Ol.Traverse.merge_siblings_preorder instrs
    ~f:(fun instr_earlier instr_later ->
      match (instr_earlier.it, instr_later.it) with
      | ( RelAssertI
            {
              call = call_earlier;
              expect = true;
              iterexps = iterexps_earlier;
              _;
            },
          RelAssertI
            {
              call = call_later;
              expect = true;
              iterexps = iterexps_later;
              block = block_later;
            } ) ->
          if
            Sl.Eq.eq_relcall call_later call_earlier
            && Sl.Eq.eq_iterexps iterexps_later iterexps_earlier
          then Take (merge_into_block instr_earlier ~later:block_later)
          else Skip
      | _ -> Stop)

(* Case analysis insertion *)

let merge_case (case : case) ~into:(cases : case list) ~(exp : exp)
    ~(total : bool) ~(from_earlier : bool) ~(senv : SEnv.t) : case list option =
  let rec merge_case' (cases_skipped : case list) (cases : case list) =
    match cases with
    | [] when total -> assert false
    | [] -> Some (List.rev_append cases_skipped [ case ])
    | ((guard_h, _) as case_h) :: cases_t -> (
        (* Reversing [overlap_guard]'s arguments can change its result. *)
        let (guard_earlier, block_earlier), (guard_later, block_later) =
          if from_earlier then (case, case_h) else (case_h, case)
        in
        match overlap_guard senv exp guard_earlier guard_later with
        | Identical ->
            (* The default SL evaluator uses the first result in a block. *)
            let case_h = (guard_h, merge_block block_earlier block_later) in
            Some (List.rev_append cases_skipped (case_h :: cases_t))
        | Disjoint _ | Partition _ ->
            merge_case' (case_h :: cases_skipped) cases_t
        | Fuzzy -> None)
  in
  merge_case' [] cases

let casify_sibling ~(senv : SEnv.t) (instr_earlier : instr)
    (instr_later : instr) : Ol.Traverse.sibling_decision =
  let at = instr_earlier.at in
  let sibling_decision_of_option (instr_opt : instr option) :
      Ol.Traverse.sibling_decision =
    match instr_opt with Some instr -> Take instr | None -> Skip
  in
  match (instr_earlier.it, instr_later.it) with
  | ( IfI (exp_cond_earlier, [], instrs_then_earlier),
      IfI (exp_cond_later, [], instrs_then_later) ) ->
      let case_analysis exp guard_earlier guard_later ~total =
        let cases =
          [
            (guard_earlier, instrs_then_earlier);
            (guard_later, instrs_then_later);
          ]
        in
        CaseI (exp, cases, total) $ at
      in
      (match overlap_exp senv exp_cond_earlier exp_cond_later with
      | Disjoint (exp, guard_earlier, guard_later) ->
          Some (case_analysis exp guard_earlier guard_later ~total:false)
      | Partition (exp, guard_earlier, guard_later) ->
          Some (case_analysis exp guard_earlier guard_later ~total:true)
      | _ -> None)
      |> sibling_decision_of_option
  | ( IfI (exp_cond_earlier, [], instrs_then_earlier),
      CaseI (exp_later, cases_later, total_later) ) ->
      Option.bind (exp_as_guard exp_later exp_cond_earlier)
        (fun guard_earlier ->
          merge_case
            (guard_earlier, instrs_then_earlier)
            ~into:cases_later ~exp:exp_later ~total:total_later
            ~from_earlier:true ~senv)
      |> Option.map (fun cases -> CaseI (exp_later, cases, total_later) $ at)
      |> sibling_decision_of_option
  | ( CaseI (exp_earlier, cases_earlier, total_earlier),
      IfI (exp_cond_later, [], instrs_then_later) ) ->
      Option.bind (exp_as_guard exp_earlier exp_cond_later) (fun guard_later ->
          merge_case
            (guard_later, instrs_then_later)
            ~into:cases_earlier ~exp:exp_earlier ~total:total_earlier
            ~from_earlier:false ~senv)
      |> Option.map (fun cases -> CaseI (exp_earlier, cases, false) $ at)
      |> sibling_decision_of_option
  | ( CaseI (exp_earlier, cases_earlier, total_earlier),
      CaseI (exp_later, cases_later, _) ) ->
      (if Sl.Eq.eq_exp exp_earlier exp_later then
         List.fold_left
           (fun cases_opt case_later ->
             Option.bind cases_opt (fun cases ->
                 merge_case case_later ~into:cases ~exp:exp_earlier
                   ~total:total_earlier ~from_earlier:false ~senv))
           (Some cases_earlier) cases_later
       else None)
      |> Option.map (fun cases ->
             CaseI (exp_earlier, cases, total_earlier) $ at)
      |> sibling_decision_of_option
  | _ -> Stop

let casify (senv : SEnv.t) (instrs : instr list) : instr list =
  Ol.Traverse.merge_siblings_preorder instrs ~f:(casify_sibling ~senv)

(* Totalize case analysis of variant matches *)

let find_variant_case_analysis (senv : SEnv.t) (cases : case list) :
    mixop list option =
  List.fold_left
    (fun mixops_opt (guard, _) ->
      match mixops_opt with
      | Some mixops -> (
          match guard with
          | SubG typ ->
              Some (mixops @ (typ |> typ_as_variant senv |> Option.get))
          | MatchG (CaseP mixop) -> Some (mixops @ [ mixop ])
          | _ -> None)
      | None -> None)
    (Some []) cases

let totalize_case_analysis (senv : SEnv.t) (instrs : instr list) : instr list =
  Ol.Traverse.rewrite_instrs_postorder instrs ~f:(fun instr ->
      match instr.it with
      | CaseI (exp, cases, false) -> (
          match find_variant_case_analysis senv cases with
          | Some mixops_case ->
              let module Set = Set.Make (struct
                type t = Il.Mixfix.mixop

                let compare = Il.Mixfix.compare_mixop
              end) in
              let mixops_total =
                let typ = exp.note.typ $ exp.at in
                typ |> typ_as_variant senv |> Option.get
              in
              let mixops_total = Set.of_list mixops_total in
              let mixops_case = Set.of_list mixops_case in
              let total = Set.equal mixops_case mixops_total in
              { instr with it = CaseI (exp, cases, total) }
          | None -> instr)
      | _ -> instr)

(* Apply optimizations until it reaches a fixed point *)

let optimize_pre (instrs : instr list) : instr list =
  instrs |> remove_let_alias |> matchify_if_eq_terminals

let rec optimize_loop (rtenv : RTEnv.t) (senv : SEnv.t) (instrs : instr list) :
    instr list =
  let instrs_optimized =
    instrs |> merge_binding rtenv |> merge_if senv |> merge_rel_assert
    |> casify senv
  in
  if Ol.Eq.eq_instrs instrs instrs_optimized then instrs
  else optimize_loop rtenv senv instrs_optimized

let optimize_post (senv : SEnv.t) (instrs : instr list) : instr list =
  instrs |> totalize_case_analysis senv

let optimize (rtenv : RTEnv.t) (senv : SEnv.t) (instrs : instr list) :
    instr list =
  instrs |> optimize_pre |> optimize_loop rtenv senv |> optimize_post senv
