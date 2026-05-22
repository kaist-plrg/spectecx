(** Per-anchor key/value store with used-set tracking.

    Allocated locally by {!Driver.run}; the [used] mutation cannot escape that
    call. *)

type t

val create : (string * string) list -> t
val cardinal : t -> int
val find_opt : t -> string -> string option
val mark_used : t -> string -> unit
val is_used : t -> string -> bool

(** Keys that were registered but never marked used, sorted alphabetically. *)
val unused : t -> string list
