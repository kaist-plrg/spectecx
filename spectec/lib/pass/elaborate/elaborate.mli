type error = Diag.t list

(** Returns the definitions, rules and clauses accepted by elaboration, together
    with the errors it reports. *)
val elab_spec : Lang.El.spec -> Lang.Il.spec * error
