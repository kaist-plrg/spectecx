(** [unparse ~spec inputs] prints one ITPL input as source text. [spec] is
    unused.

    @raise Invalid_argument
      if [inputs] does not contain exactly one input or a constructor has no
      ITPL source form.
    @raise Failure if a value has an unexpected IL representation. *)
val unparse : spec:Lang.Il.spec -> Lang.Il.Value.t list -> string
