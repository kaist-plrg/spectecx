(** Splice anchors. *)

type frame = { name : string; prefix : string; suffix : string; header : bool }

let prefix_source =
  "ifdef::backend-html5[]\n"
  ^ ".Click to view the specification source\n[%collapsible]\n====\n----\n"

let suffix_source = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\n" ^ "endif::[]"
let prefix_prose = "****\n"
let suffix_prose = "\n****"

type 'spec entry = { frame : frame; extract : 'spec -> (string * string) list }
