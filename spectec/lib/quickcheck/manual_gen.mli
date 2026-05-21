open Lang.Il

(** [Some gen] if a manual generator is registered for generator [name], [None]
    otherwise. The generator produces bindings for ALL free variables at once,
    allowing correlated generation. *)
val gen_inputs : spec -> string -> (string * value) list Gen.t option
