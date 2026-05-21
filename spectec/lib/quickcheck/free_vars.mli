open Lang.Il

(** Free input variables of a premise list: ids appearing in input positions of
    rule premises (or in [if]/[let]/etc. expressions) that are not bound by any
    premise in the list. *)
val of_premises : core_spec:spec -> prem list -> (id * typ) list
