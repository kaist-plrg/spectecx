(* Event dispatcher for instrumentation handlers.

   Manages handler registration and event dispatch to all registered handlers.
   Called from interpreters to notify handlers of execution events.

   Usage:
   1. Runner calls: set_handlers [handler1; handler2]
   2. Runner calls: init ~spec
   3. Interpreters call: notify_rel_enter ~id ~at ~values
   4. All handlers receive the event
   5. Runner calls: finish ()
*)

(* Handler state - set by runner before interpretation *)

let handlers : (module Handler.S) list ref = ref []
let set_handlers hs = handlers := hs

(* Event dispatchers called from interpreters *)

let init ~spec =
  (* Initialize handlers *)
  if !handlers <> [] then
    List.iter (fun (module H : Handler.S) -> H.init ~spec) !handlers

(* Test lifecycle events - called by runner for each test case *)
let notify_test_start ~test_case_id =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_test_start ~test_case_id)
      !handlers

let notify_test_end ~test_case_id =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_test_end ~test_case_id)
      !handlers

let notify_rel_enter ~id ~at ~values =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_rel_enter ~id ~at ~values)
      !handlers

let notify_rel_exit ~id ~at ~success =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_rel_exit ~id ~at ~success)
      !handlers

let notify_rule_enter ~id ~rule_id ~at =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_rule_enter ~id ~rule_id ~at)
      !handlers

let notify_rule_exit ~id ~rule_id ~at ~success =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_rule_exit ~id ~rule_id ~at ~success)
      !handlers

let notify_func_enter ~id ~at ~values =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_func_enter ~id ~at ~values)
      !handlers

let notify_func_exit ~id ~at =
  if !handlers <> [] then
    List.iter (fun (module H : Handler.S) -> H.on_func_exit ~id ~at) !handlers

let notify_clause_enter ~id ~clause_idx ~at =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_clause_enter ~id ~clause_idx ~at)
      !handlers

let notify_clause_exit ~id ~clause_idx ~at ~success =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) ->
        H.on_clause_exit ~id ~clause_idx ~at ~success)
      !handlers

let notify_iter_prem_enter ~prem ~at =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_iter_prem_enter ~prem ~at)
      !handlers

let notify_iter_prem_exit ~at =
  if !handlers <> [] then
    List.iter (fun (module H : Handler.S) -> H.on_iter_prem_exit ~at) !handlers

let notify_prem_enter ~prem ~at =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_prem_enter ~prem ~at)
      !handlers

let notify_prem_exit ~prem ~at ~success =
  if !handlers <> [] then
    List.iter
      (fun (module H : Handler.S) -> H.on_prem_exit ~prem ~at ~success)
      !handlers

let notify_instr ~instr ~at =
  if !handlers <> [] then
    List.iter (fun (module H : Handler.S) -> H.on_instr ~instr ~at) !handlers

let finish () = List.iter (fun (module H : Handler.S) -> H.finish ()) !handlers
