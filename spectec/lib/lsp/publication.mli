type t

val empty : t

(** Return updated publication state and reports for affected files. Reports
    combine diagnostics from overlapping specs and include empty lists for files
    whose diagnostics must be cleared. *)
val update :
  ?closed:Sources.scope ->
  t ->
  Workspace.analysis list ->
  t * (string * Linol_eio.Diagnostic.t list) list
