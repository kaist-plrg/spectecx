module Il = Lang.Il
module Sl = Lang.Sl

(** The spec value shared between static analyses and {!Handler}. *)
type spec = IlSpec of Il.spec | SlSpec of Sl.spec

(** A static analysis runs once at session init and is shared across all
    handlers that depend on it. *)
module type S = sig
  type export_data

  val name : string
  val init : spec -> unit
  val reset : unit -> unit

  (** {1 Optional checkpointing}

      Analyses that aren't checkpointable return [None] from [export]; [restore]
      is then never called on them. *)

  val export : unit -> export_data option
  val restore : export_data -> unit
end

(** Idempotent, so handlers can declare overlapping static dependencies without
    coordination. *)
val register : (module S) -> unit

val get : string -> (module S) option
val init_all : spec -> unit
val reset_all : unit -> unit

(** Non-checkpointable analyses are silently skipped. *)
val export_all : unit -> (string * Marshal.extern_flags list * bytes) list

(** Raises if [name] is not registered. *)
val restore : string -> bytes -> unit
