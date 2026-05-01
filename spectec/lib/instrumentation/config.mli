(** Active instrumentation configuration — the list of handlers selected by
    parsing CLI flags against {!Instrumentation_core.Descriptor.S}. *)

open Instrumentation_core

type t = Descriptor.active_handler list

val default : t

(** Extract the handler modules. As a side effect, registers every handler's
    static dependencies so {!Instrumentation_static.Static.init_all} sees them.
*)
val to_handlers : t -> (module Handler.S) list

(** [Error msg] when any configured handler is incompatible with the chosen
    interpreter mode ([sl_mode] selects SL vs IL). *)
val validate_mode : t -> sl_mode:bool -> (unit, string) result

val close_outputs : t -> unit
