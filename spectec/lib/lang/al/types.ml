open Common.Annotated

[@@@ocamlformat "disable"]
[@@@warning "-30"]  (* Several AST records intentionally share semantic field names. *)

type id = Il.id
type hint = Il.hint
type typ = Il.typ
type reltyp = Il.reltyp
type deftyp = Il.deftyp
type exp = Il.exp
type notexp = Il.notexp
type arg = Il.arg
type param = Il.param
type tparam = Il.tparam
type iter = Il.iter
type var = Il.var
type iterexp = Il.iterexp

(* Each run of an iterated premise reads [vars_input] and binds
   [vars_output]. *)
type iterinstr = { iter : iter; vars_input : var list; vars_output : var list }

let iterexp_of_iterinstr ({ iter; vars_input; vars_output } : iterinstr) :
    iterexp =
  (iter, vars_input @ vars_output)

(* Rules *)

type rule = rule' located
and rule' = { ruleid : id; concl : notexp; prems : prem list }

(* Clauses *)

and clause = clause' located
and clause' = { args : arg list; body : exp; prems : prem list }

(* Premises *)

and relcall = Il.relcall = { relid : id; notexp : notexp }

and prem = (prem', prem_prov option) annotated
and prem_prov =
  | Source of El.prem  (* lowered from this EL premise *)
  | Synthesized        (* introduced by the algo pass, with no EL source *)
and prem' =
  | RelPr of relcall               (* id `:` notexp *)
  | RelAssertPr of { call : relcall; expect : bool }
                                   (* `if` id `:` notexp `holds` when expect, else `does not hold` *)
  | IfPr of { cond : exp; role : if_role }  (* `if` exp *)
  | ElsePr                         (* `otherwise` *)
  | LetPr of exp * exp             (* `let` exp `=` exp *)
  | IterPr of prem * iterinstr     (* prem iterinstr *)
  | DebugPr of exp                 (* `debug` exp *)

and if_role =
  | Condition
  | Guard  (* failing it means the rule does not apply *)

(* Definitions *)

type def = def' located
and def' =
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp *)
  | TypD of { synid : id; tparams : tparam list; deftyp : deftyp }
  (* `relation` id `:` reltyp rule* *)
  | RelD of { relid : id; reltyp : reltyp; rules : rule list }
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ clause* *)
  | DecD of { defid : id; tparams : tparam list; params : param list; typ : typ; clauses : clause list }
  (* `builtin` `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ hint* *)
  | BuiltinDecD of { defid : id; tparams : tparam list; params : param list; typ : typ; hints : hint list }

(* Spec *)

type spec = def list
