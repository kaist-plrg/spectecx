(** Engine values as DAP sees them. *)

(** The relation ([Rel]) or rule ([Rel/rule]) a function breakpoint names. *)
val target_of_name : string -> Engine.target option

(** Where a frame is shown, as stack frame fields: its current premise, or else
    its rule, clause or definition. Source paths are canonical
    ({!Engine.canonical}). *)
val position : Session.t -> Engine.frame -> (string * Yojson.Safe.t) list

(** The body of the [stopped] event for a stop. *)
val stopped : Engine.stop_reason -> Yojson.Safe.t
