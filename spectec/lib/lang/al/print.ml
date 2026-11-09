open Il.Print
open Types
open Common.Annotated
module Location = Common.Location

(* Iterations *)

let string_of_iterinstr iterinstr =
  let iter, vars = iterexp_of_iterinstr iterinstr in
  let vars =
    List.sort
      (fun (var_a : Il.var) (var_b : Il.var) ->
        String.compare var_a.varid.it var_b.varid.it)
      vars
  in
  string_of_iterexp (iter, vars)

let string_of_iterinstrs iterinstrs =
  iterinstrs |> List.map string_of_iterinstr |> String.concat ""

(* Rules *)

let rec string_of_rule rule =
  let { ruleid; concl; prems } = rule.it in
  ";; " ^ Location.to_string rule.at ^ ":\n   rule " ^ string_of_ruleid ruleid
  ^ ": " ^ string_of_notexp concl ^ string_of_prems prems

and string_of_rules rules =
  String.concat ""
    (List.map (fun rule -> "\n\n   " ^ string_of_rule rule) rules)

(* Clause *)

and string_of_clause idx clause =
  let { args; body; prems } = clause.it in
  ";; "
  ^ Location.to_string clause.at
  ^ ":\n   clause " ^ string_of_int idx ^ string_of_args args ^ " = "
  ^ string_of_exp body ^ string_of_prems prems

and string_of_clauses clauses =
  String.concat ""
    (List.mapi
       (fun idx clause -> "\n\n   " ^ string_of_clause idx clause)
       clauses)

(* Premises *)

and string_of_prem prem =
  match prem.it with
  | RelPr call -> "rel " ^ string_of_relcall call
  | RelAssertPr { call; expect } ->
      "if " ^ string_of_relcall call
      ^ if expect then " holds" else " does not hold"
  | IfPr { cond; _ } -> "if " ^ string_of_exp cond
  | ElsePr -> "otherwise"
  | LetPr (exp_l, exp_r) ->
      "let " ^ string_of_exp exp_l ^ " = " ^ string_of_exp exp_r
  | IterPr (({ it = IterPr _; _ } as prem), iterinstr) ->
      string_of_prem prem ^ string_of_iterinstr iterinstr
  | IterPr (prem, iterinstr) ->
      "(" ^ string_of_prem prem ^ ")" ^ string_of_iterinstr iterinstr
  | DebugPr exp -> "debug " ^ string_of_exp exp

and string_of_prems prems =
  String.concat ""
    (List.map (fun prem -> "\n      -- " ^ string_of_prem prem) prems)

(* Definitions *)

let rec string_of_def def =
  ";; " ^ Location.to_string def.at ^ ":\n"
  ^
  match def.it with
  | TypD { synid = id; deftyp = { it = Il.OpaqueT; _ }; _ } ->
      "opaque syntax " ^ string_of_typid id
  | TypD { synid; tparams; deftyp } ->
      "syntax " ^ string_of_typid synid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | RelD { relid; reltyp; rules } ->
      "relation " ^ string_of_relid relid ^ ": " ^ string_of_reltyp reltyp
      ^ string_of_rules rules
  | BuiltinDecD { defid; tparams; params; typ; _ } ->
      "builtin dec " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ
  | DecD { defid; tparams; params; typ; clauses } ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ ^ " ="
      ^ string_of_clauses clauses

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
