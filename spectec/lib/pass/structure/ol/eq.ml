module Sl = Lang.Sl
open Ast

(* Cases *)

let rec eq_case (case_a : case) (case_b : case) : bool =
  let guard_a, instrs_a = case_a in
  let guard_b, instrs_b = case_b in
  Sl.Eq.eq_guard guard_a guard_b && eq_instrs instrs_a instrs_b

and eq_cases (cases_a : case list) (cases_b : case list) : bool =
  List.length cases_a = List.length cases_b
  && List.for_all2 eq_case cases_a cases_b

(* Instructions *)

and eq_instr (instr_a : instr) (instr_b : instr) : bool =
  match (instr_a.it, instr_b.it) with
  | ( RelI { call = call_a; iterinstrs = iterinstrs_a; block = block_a },
      RelI { call = call_b; iterinstrs = iterinstrs_b; block = block_b } ) ->
      Sl.Eq.eq_relcall call_a call_b
      && Sl.Eq.eq_iterinstrs iterinstrs_a iterinstrs_b
      && eq_instrs block_a block_b
  | ( RelAssertI
        {
          call = call_a;
          expect = expect_a;
          iterexps = iterexps_a;
          block = block_a;
        },
      RelAssertI
        {
          call = call_b;
          expect = expect_b;
          iterexps = iterexps_b;
          block = block_b;
        } ) ->
      Sl.Eq.eq_relcall call_a call_b
      && Bool.equal expect_a expect_b
      && Sl.Eq.eq_iterexps iterexps_a iterexps_b
      && eq_instrs block_a block_b
  | ( IfI (exp_cond_a, iterexps_a, instrs_then_a),
      IfI (exp_cond_b, iterexps_b, instrs_then_b) ) ->
      Sl.Eq.eq_exp exp_cond_a exp_cond_b
      && Sl.Eq.eq_iterexps iterexps_a iterexps_b
      && eq_instrs instrs_then_a instrs_then_b
  | CaseI (exp_a, cases_a, total_a), CaseI (exp_b, cases_b, total_b) ->
      Sl.Eq.eq_exp exp_a exp_b && eq_cases cases_a cases_b && total_a = total_b
  | ( LetI (exp_l_a, exp_r_a, iterinstrs_a, block_a),
      LetI (exp_l_b, exp_r_b, iterinstrs_b, block_b) ) ->
      Sl.Eq.eq_exp exp_l_a exp_l_b
      && Sl.Eq.eq_exp exp_r_a exp_r_b
      && Sl.Eq.eq_iterinstrs iterinstrs_a iterinstrs_b
      && eq_instrs block_a block_b
  | ResultI exps_a, ResultI exps_b -> Sl.Eq.eq_exps exps_a exps_b
  | ReturnI exp_a, ReturnI exp_b -> Sl.Eq.eq_exp exp_a exp_b
  | DebugI (exp_a, instr_a), DebugI (exp_b, instr_b) ->
      Sl.Eq.eq_exp exp_a exp_b && eq_instr instr_a instr_b
  | _ -> false

and eq_instrs (instrs_a : instr list) (instrs_b : instr list) : bool =
  List.length instrs_a = List.length instrs_b
  && List.for_all2 eq_instr instrs_a instrs_b
