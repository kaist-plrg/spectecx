(** Minimal ANSI escape code helpers. *)

type style = Bold | Dim | Red | Yellow | Blue | Cyan | Green

(** ANSI configuration. When disabled, {!style} is the identity, so call sites
    can render the same layout regardless of whether color is wanted. *)
type t

val plain : t
val color : t

(** [style ansi styles s] wraps [s] with the given styles followed by a single
    reset, or returns [s] unchanged when [ansi] is {!plain} or [styles] is
    empty. *)
val style : t -> style list -> string -> string
