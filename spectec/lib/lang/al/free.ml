open Il.Free
open Types
open Common.Annotated

(* Premises *)

let rec free_prem (prem : prem) : t =
  match prem.it with
  | RelPr call -> free_relcall call
  | RelAssertPr { call; _ } -> free_relcall call
  | IfPr { cond; _ } -> free_exp cond
  | LetPr (exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | ElsePr -> empty
  | IterPr (prem, _) -> free_prem prem
  | DebugPr exp -> free_exp exp

and free_prems (prems : prem list) : t =
  prems |> List.map free_prem |> List.fold_left ( + ) empty

(* Definitions *)

let free_rule (rule : rule) : t =
  let { concl; prems; _ } = rule.it in
  free_exps (Il.Mixfix.args concl) + free_prems prems

let free_clause (clause : clause) : t =
  let { args; body; prems } = clause.it in
  free_args args + free_exp body + free_prems prems
