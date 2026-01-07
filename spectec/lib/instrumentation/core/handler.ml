(* Handler signatures for instrumentation.

   Defines:
   - S: Base handler signature with lifecycle and event callbacks
   - S_with_data: Extended signature for handlers that support data export/restore

   Handlers are registered with Dispatcher and receive events from interpreters.
*)

open Common.Source
module Il = Lang.Il
module Sl = Lang.Sl

(* Spec type passed to handlers at init *)
type spec = IlSpec of Il.spec | SlSpec of Sl.spec

(* Base handler signature *)
module type S = sig
  val init : spec:spec -> unit

  (* Common events *)
  val on_rel_enter : id:string -> at:region -> values:Il.Value.t list -> unit
  val on_rel_exit : id:string -> at:region -> success:bool -> unit
  val on_rule_enter : id:string -> rule_id:string -> at:region -> unit

  val on_rule_exit :
    id:string -> rule_id:string -> at:region -> success:bool -> unit

  val on_func_enter : id:string -> at:region -> values:Il.Value.t list -> unit
  val on_func_exit : id:string -> at:region -> unit
  val on_clause_enter : id:string -> clause_idx:int -> at:region -> unit

  val on_clause_exit :
    id:string -> clause_idx:int -> at:region -> success:bool -> unit

  (* IL-specific events *)
  val on_iter_prem_enter : prem:Il.prem -> at:region -> unit
  val on_iter_prem_exit : at:region -> unit
  val on_prem_enter : prem:Il.prem -> at:region -> unit
  val on_prem_exit : prem:Il.prem -> at:region -> success:bool -> unit

  (* SL-specific events *)
  val on_instr : instr:Sl.instr -> at:region -> unit
  val finish : unit -> unit
end

(* Extended handler that can export and restore collected data.
   Use this when backends need structured access to instrumentation results
   instead of just file/stdout output.
   - get_result: export current state for programmatic access or checkpointing
   - restore: reload state from a previous result (for checkpoint resume) *)
module type S_with_data = sig
  include S

  type result

  val get_result : unit -> result
  val restore : result -> unit
end
