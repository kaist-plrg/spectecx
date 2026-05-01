open Lang.Il

val gen_inputs :
  spec -> Qc_ir.ir_var list -> (string * value) list Gen.t option
(** [Some gen] if a manual generator is registered for this variable list,
    [None] otherwise. The generator produces bindings for ALL free variables
    at once, allowing correlated generation. *)
