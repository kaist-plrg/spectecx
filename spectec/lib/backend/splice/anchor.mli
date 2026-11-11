(** Splice anchors. *)

type frame = {
  name : string;
  prefix : string;
      (** The driver emits [prefix] before the rendered text of an anchor's
          keys. *)
  suffix : string;
      (** The driver emits [suffix] after the rendered text of an anchor's keys.
      *)
  header : bool;
      (** If [true], the driver emits [[[key]]] before the prefix on the first
          anchor that resolves a given key. *)
}

val prefix_source : string
val suffix_source : string
val prefix_prose : string
val suffix_prose : string

type 'spec entry = {
  frame : frame;
  extract : 'spec -> (string * string) list;
      (** Returns [(key, rendered_text)] pairs. *)
}
