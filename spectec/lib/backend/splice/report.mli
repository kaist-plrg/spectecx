(** Final report describing keys that were registered but never referenced by
    any anchor. *)

type per_anchor = { name : string; total : int; unused : string list }
type t = per_anchor list

val of_stores : (string * Store.t) list -> t

(** Renders the report as text suitable for a [.missing] sidecar file: one
    section per anchor name, listing every unused key. *)
val to_string : t -> string
