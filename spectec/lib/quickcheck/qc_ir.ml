open Lang.Il

type ir_var = { iv_id : id'; iv_typ : typ }

type synth_rel = {
  sr_id : id'; (* synthetic relation name, e.g. "__qc_foo_prems__" *)
  sr_inputs : id' list; (* names of variables passed as inputs *)
  sr_outputs : (id' * typ) list;
      (* names+types of variables returned as outputs *)
}

type qc_command =
  | QcProp of {
      name : string;
      free_vars : ir_var list;
      generator : string option;
      prems_rel : synth_rel;
      goal_rel : synth_rel;
    }
  | QcGen of {
      name : string;
      free_vars : ir_var list;
      generator : string option;
      prems_rel : synth_rel;
    }

type t = qc_command list
