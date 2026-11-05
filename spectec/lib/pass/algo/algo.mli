type error = Diag.t list

(** Decides which variable occurrences bind values and which read them. Adds
    pattern checks and annotates iterations.

    Expects elaborated IL with empty variable lists in expression iterations.
    Omits rules and clauses that fail analysis and returns their errors. *)
val algo_spec : Lang.Il.spec -> Lang.Il.spec * error
