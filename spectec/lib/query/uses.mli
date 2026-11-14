(** Uses of names inside a spec's definitions, at the places where they are
    written. The name a top-level definition introduces is not a use at its
    declaration site. A recursive call counts as a use of the function.

    [of_spec] leaves out the atoms of a syntax case or a relation's notation, a
    struct type's field names, a rule header's relation, a clause head's
    function name, a function parameter's name and the type parameters in a
    definition's header. It also leaves out the expressions inside hints.
    Outside hints, variables are recorded wherever they appear, including where
    a premise or clause head binds them. *)

(** [owner] is the top-level definition that contains the use. *)
type use = private {
  owner : Definitions.definition;
  location : Common.Location.t;
}

type t

val of_spec : Lang.El.spec -> t

(** [find_all uses ~name] lists the uses of [name] in spec order. A query for a
    variable's base name also matches its suffixed forms. For example, a query
    for [e] finds uses of [e], [e_1] and [e']. *)
val find_all : t -> name:string -> use list
