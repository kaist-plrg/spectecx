open Lang.Il

val gen_inputs :
  spec -> string -> (string * value) list Gen.t option
(** [Some gen] if a manual generator is registered for [name], [None]
    otherwise. The generator produces bindings for ALL free variables at once,
    allowing correlated generation.

    Registered names:
    - ["base_prog"]        — well-typed Impty program (INT and BOOL only)
    - ["closure_prog"]     — well-typed Impty program with first-class functions
    - ["closure_fun_prog"] — program exercising curried closure calls, targeting
                             the [env_clo] evaluation rule *)
