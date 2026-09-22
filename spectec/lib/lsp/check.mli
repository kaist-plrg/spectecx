(** Return parse and elaboration diagnostics for the supplied sources. *)
val analyze : Sources.t -> Spectec.Diagnostic.t list

(** Return diagnostics grouped by source file path. Files without diagnostics
    are omitted. *)
val diagnostics :
  Spectec.Diagnostic.t list -> (string * Linol_eio.Diagnostic.t list) list
