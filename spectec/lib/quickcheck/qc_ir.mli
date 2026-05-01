open Lang.Il

type binding_origin =
  | Free
  | BoundByRule of string * int
  | BoundByLet of exp

type ir_var = {
  iv_id     : string;
  iv_typ    : typ;
  iv_origin : binding_origin;
}

type qc_command =
  | QcProp of {
      free_vars : ir_var list;
      goal      : prem;
      prems     : prem list;
    }
  | QcGen of {
      free_vars : ir_var list;
      prems     : prem list;
    }

type t = qc_command list
