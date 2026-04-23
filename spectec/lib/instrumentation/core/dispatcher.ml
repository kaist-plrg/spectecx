let handlers : (module Handler.S) list ref = ref []
let iter_handlers f = if !handlers <> [] then List.iter f !handlers

let init ~spec ~handlers:hs =
  handlers := hs;
  iter_handlers (fun (module H : Handler.S) -> H.init ~spec)

let notify_test_start ~test_case_id =
  iter_handlers (fun (module H : Handler.S) -> H.on_test_start ~test_case_id)

let notify_test_end ~test_case_id =
  iter_handlers (fun (module H : Handler.S) -> H.on_test_end ~test_case_id)

let notify_rel_enter ~id ~at ~values =
  iter_handlers (fun (module H : Handler.S) -> H.on_rel_enter ~id ~at ~values)

let notify_rel_exit ~id ~at ~success =
  iter_handlers (fun (module H : Handler.S) -> H.on_rel_exit ~id ~at ~success)

let notify_rule_enter ~id ~rule_id ~at =
  iter_handlers (fun (module H : Handler.S) -> H.on_rule_enter ~id ~rule_id ~at)

let notify_rule_exit ~id ~rule_id ~at ~success =
  iter_handlers (fun (module H : Handler.S) ->
      H.on_rule_exit ~id ~rule_id ~at ~success)

let notify_func_enter ~id ~at ~values =
  iter_handlers (fun (module H : Handler.S) -> H.on_func_enter ~id ~at ~values)

let notify_func_exit ~id ~at =
  iter_handlers (fun (module H : Handler.S) -> H.on_func_exit ~id ~at)

let notify_clause_enter ~id ~clause_idx ~at =
  iter_handlers (fun (module H : Handler.S) ->
      H.on_clause_enter ~id ~clause_idx ~at)

let notify_clause_exit ~id ~clause_idx ~at ~success =
  iter_handlers (fun (module H : Handler.S) ->
      H.on_clause_exit ~id ~clause_idx ~at ~success)

let notify_iter_prem_enter ~prem ~at =
  iter_handlers (fun (module H : Handler.S) -> H.on_iter_prem_enter ~prem ~at)

let notify_iter_prem_exit ~at =
  iter_handlers (fun (module H : Handler.S) -> H.on_iter_prem_exit ~at)

let notify_prem_enter ~prem ~at =
  iter_handlers (fun (module H : Handler.S) -> H.on_prem_enter ~prem ~at)

let notify_prem_exit ~prem ~at ~success =
  iter_handlers (fun (module H : Handler.S) ->
      H.on_prem_exit ~prem ~at ~success)

let notify_instr ~instr ~at =
  iter_handlers (fun (module H : Handler.S) -> H.on_instr ~instr ~at)

let finish () = List.iter (fun (module H : Handler.S) -> H.finish ()) !handlers
