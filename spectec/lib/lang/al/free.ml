open Il.Free
open Types
open Common.Annotated

(* Instructions *)

let rec free_instr (instr : instr) : t =
  match instr.it with
  | RelI call -> free_relcall call
  | RelAssertI { call; _ } -> free_relcall call
  | IfI { cond; _ } -> free_exp cond
  | LetI (exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | OtherwiseI -> empty
  | IterI (instr, _) -> free_instr instr
  | DebugI exp -> free_exp exp

and free_instrs (instrs : instr list) : t =
  instrs |> List.map free_instr |> List.fold_left ( + ) empty

(* Definitions *)

let free_rule (rule : rule) : t =
  let { concl; instrs; _ } = rule.it in
  free_exps (Il.Mixfix.args concl) + free_instrs instrs

let free_clause (clause : clause) : t =
  let { args; body; instrs } = clause.it in
  free_args args + free_exp body + free_instrs instrs
