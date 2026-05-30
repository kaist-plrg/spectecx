(** Splice anchors.

    An anchor binds an anchor name to a frame (text inserted around the rendered
    content) and an [extract] function that produces [(key, rendered_text)]
    pairs from a spec. Two flavours: [Source.entry] extracts from an [El.spec],
    [Prose.entry] extracts from a [Pl.spec]. *)

type frame = { name : string; prefix : string; suffix : string; header : bool }

let prefix_source =
  "ifdef::backend-html5[]\n"
  ^ ".Click to view the specification source\n[%collapsible]\n====\n----\n"

let suffix_source = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\n" ^ "endif::[]"
let prefix_prose = "****\n"
let suffix_prose = "\n****"

module Source = struct
  type entry = {
    frame : frame;
    extract : Lang.El.spec -> (string * string) list;
  }
end

module Prose = struct
  type entry = {
    frame : frame;
    extract : Lang.Pl.spec -> (string * string) list;
  }
end
