open Lang.Il

type ir_var = {
  iv_id  : string;
  iv_typ : typ;
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
