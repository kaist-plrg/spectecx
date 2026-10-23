(** An [opaque syntax] declaration leaves its value representation to the
    target. This module lets specifications carry OCaml values that target
    functions can inspect. *)

(** A target's type definition for an opaque declaration, with OCaml values of
    type ['a]. *)
type 'a typdef

(** Hides the OCaml type parameter so different definitions can share a list. *)
type typdef_as_any = Typdef : 'a typdef -> typdef_as_any

(** A type definition and an OCaml value of that type, with the OCaml type
    hidden. *)
type value

(** Each call creates a distinct definition for [name]. [compare] must
    consistently order values. Values that compare equal must be interchangeable
    in target operations and have the same hash, since the interpreter may reuse
    cached results. *)
val make_typdef :
  name:string -> compare:('a -> 'a -> int) -> hash:('a -> int) -> 'a typdef

(** The declaration name supplied to [make_typdef]. *)
val typdef_name : 'a typdef -> string

(** The stored value's opaque type name. *)
val name : value -> string

(** Stores the value without copying. Use immutable values to preserve the
    interpreter's hashes and cached results. *)
val of_ocaml : typdef:'a typdef -> 'a -> value

(** Returns the OCaml value if [typdef] is its type definition, or [None]
    otherwise. *)
val to_ocaml_opt : typdef:'a typdef -> value -> 'a option

(** Tests whether [typdef] is the value's type definition. *)
val has_type : value -> typdef:'a typdef -> bool

(** Uses the supplied comparison for values of the same definition. Different
    definitions never compare equal. *)
val compare : value -> value -> int

(** Values that compare equal receive the same hash. *)
val hash : value -> int
