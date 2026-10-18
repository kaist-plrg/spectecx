open Lang.Il

(** A lookup table of the target's opaque type definitions, keyed by declaration
    name. *)
type t

(** Builds the table, rejecting duplicate names. *)
val make : Opaque.typdef_as_any list -> (t, string) result

(** Checks that [id] has an entry in the table when [deftyp] is [OpaqueT].
    Accepts other type definitions without checking the table. *)
val check_declaration : t -> id:id -> deftyp:deftyp -> (unit, string) result

(** Tests whether [value]'s type definition is the same as the table entry for
    [name]. Returns [false] if [name] is absent. *)
val has_type : t -> name:string -> value:Opaque.value -> bool

(** Checks the same condition as [has_type], returning an error if [name] is
    absent or the type definitions differ. *)
val check_type : t -> name:string -> value:Opaque.value -> (unit, string) result
