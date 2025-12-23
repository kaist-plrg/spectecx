(* Instrumentation hooks for interpreter events.

   Provides:
   - HANDLER: module type for various instrumentations
   - notify_*: dispatcher functions called from interpreter
   - set_handlers: register handlers before running interpreter

   Usage:
   1. Create handler: module MyHandler : HANDLER = struct ... end
   2. Runner calls: set_handlers [handler1; handler2]
   3. Interpreters call: notify_rel_enter ~id ~at ~values
   4. All handlers receive the event
   5. Runner calls: finish ()
*)

module Il = Lang.Il

(* Handler callback signature *)

module type HANDLER = sig
  val on_rel_enter :
    id:string -> at:Common.Source.region -> values:Il.Value.t list -> unit

  val on_rel_exit : id:string -> at:Common.Source.region -> success:bool -> unit

  val on_func_enter :
    id:string -> at:Common.Source.region -> values:Il.Value.t list -> unit

  val on_func_exit : id:string -> at:Common.Source.region -> unit
  val on_prem : at:Common.Source.region -> unit
  val on_instr : at:Common.Source.region -> unit
  val finish : unit -> unit
end

(* Handler state - set by runner before interpretation *)

let handlers : (module HANDLER) list ref = ref []
let set_handlers hs = handlers := hs

(* Event dispatchers called from interpreters *)

let notify_rel_enter ~id ~at ~values =
  List.iter
    (fun (module H : HANDLER) -> H.on_rel_enter ~id ~at ~values)
    !handlers

let notify_rel_exit ~id ~at ~success =
  List.iter
    (fun (module H : HANDLER) -> H.on_rel_exit ~id ~at ~success)
    !handlers

let notify_func_enter ~id ~at ~values =
  List.iter
    (fun (module H : HANDLER) -> H.on_func_enter ~id ~at ~values)
    !handlers

let notify_func_exit ~id ~at =
  List.iter (fun (module H : HANDLER) -> H.on_func_exit ~id ~at) !handlers

let notify_prem ~at =
  List.iter (fun (module H : HANDLER) -> H.on_prem ~at) !handlers

let notify_instr ~at =
  List.iter (fun (module H : HANDLER) -> H.on_instr ~at) !handlers

let finish () = List.iter (fun (module H : HANDLER) -> H.finish ()) !handlers

(* No-op default handler *)

module Noop : HANDLER = struct
  let on_rel_enter ~id:_ ~at:_ ~values:_ = ()
  let on_rel_exit ~id:_ ~at:_ ~success:_ = ()
  let on_func_enter ~id:_ ~at:_ ~values:_ = ()
  let on_func_exit ~id:_ ~at:_ = ()
  let on_prem ~at:_ = ()
  let on_instr ~at:_ = ()
  let finish () = ()
end
