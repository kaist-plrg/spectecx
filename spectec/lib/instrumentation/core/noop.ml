(* No-op default handler - implements Handler.S with empty implementations.

   Useful as a base for handlers that only need to implement a subset of events,
   or as a placeholder when no instrumentation is needed.
*)

module M : Handler.S = struct
  let static_dependencies = []
  let init ~spec:_ = ()
  let on_test_start ~test_case_id:_ = ()
  let on_test_end ~test_case_id:_ = ()
  let on_rel_enter ~id:_ ~at:_ ~values:_ = ()
  let on_rel_exit ~id:_ ~at:_ ~success:_ = ()
  let on_rule_enter ~id:_ ~rule_id:_ ~at:_ = ()
  let on_rule_exit ~id:_ ~rule_id:_ ~at:_ ~success:_ = ()
  let on_func_enter ~id:_ ~at:_ ~values:_ = ()
  let on_func_exit ~id:_ ~at:_ = ()
  let on_clause_enter ~id:_ ~clause_idx:_ ~at:_ = ()
  let on_clause_exit ~id:_ ~clause_idx:_ ~at:_ ~success:_ = ()
  let on_iter_prem_enter ~prem:_ ~at:_ = ()
  let on_iter_prem_exit ~at:_ = ()
  let on_prem_enter ~prem:_ ~at:_ = ()
  let on_prem_exit ~prem:_ ~at:_ ~success:_ = ()
  let on_instr ~instr:_ ~at:_ = ()
  let finish () = ()
end

(* Re-export for convenience *)
let init = M.init
let on_test_start = M.on_test_start
let on_test_end = M.on_test_end
let on_rel_enter = M.on_rel_enter
let on_rel_exit = M.on_rel_exit
let on_rule_enter = M.on_rule_enter
let on_rule_exit = M.on_rule_exit
let on_func_enter = M.on_func_enter
let on_func_exit = M.on_func_exit
let on_clause_enter = M.on_clause_enter
let on_clause_exit = M.on_clause_exit
let on_iter_prem_enter = M.on_iter_prem_enter
let on_iter_prem_exit = M.on_iter_prem_exit
let on_prem_enter = M.on_prem_enter
let on_prem_exit = M.on_prem_exit
let on_instr = M.on_instr
let finish = M.finish
let static_dependencies = M.static_dependencies
