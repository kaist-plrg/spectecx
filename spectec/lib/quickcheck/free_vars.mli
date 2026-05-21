open Lang.Il

(** Free input variables of a premise list: ids appearing in input positions of
    rule premises (or in [if]/[let]/etc. expressions) that are not bound by any
    premise in the list. *)
val of_premises : core_spec:spec -> prem list -> (id * typ) list

(** Output variables bound by a premise list: ids appearing in output positions
    of rule premises or on the LHS of [let] premises. Used by the
    synthetic-relation builder to declare a relation's output slots; will become
    obsolete once premise evaluation no longer goes through a synthesized
    relation. *)
val outputs_of_premises : core_spec:spec -> prem list -> (id' * typ) list
