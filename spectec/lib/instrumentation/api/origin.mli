(** A value's position within an external input.

    The decoder chooses [label] to identify the input. The [path] lists the
    steps from the root of that input to the value. *)

type step =
  | Struct of Lang.Xl.Atom.t
  | List of int  (** List element, counted from zero. *)

type t = { label : string; path : step list }

(** The root of the input labeled [label]. *)
val make : label:string -> t

(** Append [step] to the origin's [path]. *)
val extend : t -> step:step -> t
