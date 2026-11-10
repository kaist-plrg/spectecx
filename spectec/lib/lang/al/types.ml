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

type iterinstr = { iter : iter; vars_input : var list; vars_output : var list }

let iterexp_of_iterinstr ({ iter; vars_input; vars_output } : iterinstr) :
    iterexp =
  (iter, vars_input @ vars_output)

(* Rules *)

type rule = rule' located
and rule' = { ruleid : id; concl : notexp; instrs : instr list }

(* Clauses *)

and clause = clause' located
and clause' = { args : arg list; body : exp; instrs : instr list }

(* Instructions *)

and relcall = Il.relcall = { relid : id; notexp : notexp }

and instr = (instr', origin) annotated
and origin =
  | Premise of Il.prem
  | Sidecondition of Il.prem  (* Checks for variables bound by the given
                                 premise. *)
  | Input_sidecondition       (* Checks for variables bound by inputs or
                                 arguments. *)
and instr' =
  | RelI of relcall                 (* id `:` notexp *)
  | RelAssertI of { call : relcall; expect : bool }
                                   (* `if` id `:` notexp `holds` when expect, else `does not hold` *)
  | IfI of { cond : exp; role : if_role }  (* `if` exp *)
  | OtherwiseI                     (* `otherwise` *)
  | LetI of exp * exp              (* `let` exp `=` exp *)
  | IterI of instr * iterinstr     (* instr iterinstr *)
  | DebugI of exp                  (* `debug` exp *)

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
