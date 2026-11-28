(** Engine values as DAP sees them. *)

(** Where a frame is shown, as stack frame fields: its current premise, or else
    its rule, clause or definition. Source paths are canonical
    ({!Engine.canonical}). *)
val position : Session.t -> Engine.frame -> (string * Yojson.Safe.t) list

(** The body of the [stopped] event for a stop. *)
val stopped : Engine.stop_reason -> Yojson.Safe.t
