(** Splice anchors.

    An anchor binds a name to a frame (text inserted around the rendered
    content) and an [extract] function producing [(key, rendered_text)] pairs
    from a spec. Two flavours: source-side extracts from {!Lang.El.spec},
    prose-side extracts from {!Lang.Pl.spec}. *)

type frame = {
  name : string;
  prefix : string;
  suffix : string;
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
  }
end

module Prose : sig
  type entry = {
    frame : frame;
    extract : Lang.Pl.spec -> (string * string) list;
  }
end
