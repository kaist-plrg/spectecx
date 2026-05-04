open Lang.Il

val gen_inputs :
  spec -> int -> (string * value) list Gen.t option
(** [Some gen] if a manual generator is registered for block index [i],
    [None] otherwise. The generator produces bindings for ALL free variables
    at once, allowing correlated generation. *)
