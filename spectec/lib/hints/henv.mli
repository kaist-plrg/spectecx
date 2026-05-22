(** Per-subject store of user-authored hints. Alter-kind hints sit on [Rel] or
    [Func] subjects; rel input positions (from [hint(input %N)]) are kept
    alongside so consumers can realign output positions against them. *)

type subject = Rel of El.id' | Func of El.id'
type t

val empty : t
val add_alter : t -> hid:string -> subject:subject -> Alter.t -> t
val add_rel_inputs : t -> rel:El.id' -> Input.t -> t
val find_alter : t -> hid:string -> subject:subject -> Alter.t option
val find_rel_inputs : t -> rel:El.id' -> Input.t option
val of_el_spec : El.spec -> t
