(** Splice anchors.

    Two flavours: source-side extracts from {!Lang.El.spec}, prose-side extracts
    from {!Lang.Pl.spec}. *)

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

module Source : sig
  type entry = {
    frame : frame;
    extract : Lang.El.spec -> (string * string) list;
        (** Returns [(key, rendered_text)] pairs. *)
  }
end

module Prose : sig
  type entry = {
    frame : frame;
    extract : Lang.Pl.spec -> (string * string) list;
        (** Returns [(key, rendered_text)] pairs. *)
  }
end
