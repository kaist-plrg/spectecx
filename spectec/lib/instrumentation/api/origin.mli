(** Where a value sits within an external input.

    The [source] label is chosen by whoever decodes the input, such as a task's
    [parse_input]. The [path] leads from the root of that input to the value
    through IL value structure. *)

type step =
  | Field of Lang.Xl.Atom.t  (** Struct field. *)
  | Index of int  (** List element, counted from zero. *)
  | Opt  (** Content of a present option. *)
  | Tuple of int  (** Tuple component, counted from zero. *)
  | Case of int  (** Variant case argument, counted from zero. *)

type t = { source : string; path : step list }

(** The root of the input labeled [source]. *)
val make : source:string -> t

(** The location one [step] below the given origin. *)
val extend : t -> step:step -> t

val compare : t -> t -> int

(** Renders the source followed by its path, as in [input.POINT[1]?]. *)
val to_string : t -> string

module Set : Set.S with type elt = t
