(** A text answer names each file by the filename recorded in the locations of
    the uses or definitions. A use or definition whose location is not a span is
    left out. *)

(** [render_uses uses] lists [uses] by file and by the kind and name of the
    definition that contains them, then counts them. Within each file, the
    clauses of one function share a line, and so do the unnamed rules of one
    relation. Each kind and name pair counts as one definition across files. *)
val render_uses : Uses.use list -> string
