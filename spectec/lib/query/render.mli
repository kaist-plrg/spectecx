(** A text answer names each file by the filename recorded in the locations of
    the uses or definitions. A use or definition whose location is not a span is
    left out. *)

(** [render_uses uses] lists [uses] by file and by the kind and name of the
    definition that contains them, then counts them. Within each file, the
    clauses of one function share a line, and so do the unnamed rules of one
    relation. Each kind and name pair counts as one definition across files. *)
val render_uses : Uses.use list -> string

(** [render_definitions ~text_of_file definitions] shows the source text of each
    of [definitions] under its file and line range. [text_of_file file] is the
    text of [file], or [None] when it is unknown. *)
val render_definitions :
  text_of_file:(string -> string option) -> Definitions.t -> string

(** [render_outline definitions] lists [definitions] by file, one per line with
    its line range, keyword and name. Consecutive clauses of one function share
    a line within each file. *)
val render_outline : Definitions.t -> string

(** [diagnostics_of_missing_names names] returns a bag with one error diagnostic
    for each of [names], saying that no definition has that name. *)
val diagnostics_of_missing_names : string list -> Diag.Bag.t
