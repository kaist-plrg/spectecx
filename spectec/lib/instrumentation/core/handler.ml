(** Runtime contract for instrumentation handlers.

    A handler receives lifecycle and execution events from interpreters via
    {!Dispatcher}. Handlers are plugin-style: each one also declares itself via
    {!Descriptor.S} for CLI parsing and construction. *)

open Common.Source
module Il = Lang.Il
module Sl = Lang.Sl

type spec = Instrumentation_static.Static.spec =
  | IlSpec of Il.spec
  | SlSpec of Sl.spec

(** Variant-typed events emitted by interpreters. *)
type event =
  | Test_start of { test_case_id : string }
  | Test_end of { test_case_id : string }
  | Rel_enter of { id : string; at : region; values : Il.Value.t list }
  | Rel_exit of { id : string; at : region; success : bool }
  | Rule_enter of { id : string; rule_id : string; at : region }
  | Rule_exit of { id : string; rule_id : string; at : region; success : bool }
  | Func_enter of { id : string; at : region; values : Il.Value.t list }
  | Func_exit of { id : string; at : region }
  | Clause_enter of { id : string; clause_idx : int; at : region }
  | Clause_exit of {
      id : string;
      clause_idx : int;
      at : region;
      success : bool;
    }
  | Iter_prem_enter of { prem : Il.prem; at : region }
  | Iter_prem_exit of { at : region }
  | Prem_enter of { prem : Il.prem; at : region }
  | Prem_exit of { prem : Il.prem; at : region; success : bool }
  | Instr of { instr : Sl.instr; at : region }

(** Base handler signature. Handlers pattern-match on {!event} and only need to
    act on the constructors they care about (default: [| _ -> ()]). *)
module type S = sig
  (** Static analyses this handler needs. Registered via {!Config.to_handlers}
      before runtime init. *)
  val static_dependencies : (module Instrumentation_static.Static.S) list

  val init : spec:spec -> unit
  val handle : event -> unit
  val finish : unit -> unit
end

(** Extends {!S} with structured access to collected data, for backends that
    need more than file/stdout output — e.g. checkpoint resume. *)
module type S_with_data = sig
  include S

  type result

  val get_result : unit -> result
  val restore : result -> unit
end
