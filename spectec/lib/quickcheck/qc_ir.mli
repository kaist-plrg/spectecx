open Lang.Il

type ir_var = {
  iv_id  : id';
  iv_typ : typ;
}

type synth_rel = {
  sr_id      : id';
  sr_inputs  : id' list;
  sr_outputs : (id' * typ) list;
}

type qc_command =
  | QcProp of {
      name      : string;
      free_vars : ir_var list;
      generator : string option;
      prems_rel : synth_rel;
      goal_rel  : synth_rel;
    }
  | QcGen of {
      name      : string;
      free_vars : ir_var list;
      generator : string option;
      prems_rel : synth_rel;
    }

type t = qc_command list
