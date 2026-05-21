exception StepLimitExceeded = Eval_il.StepLimitExceeded

type error = Eval_il.error

let error_to_string = Eval_il.error_to_string
let error_to_diagnostic = Eval_il.error_to_diagnostic

module Ctx = Eval_il.Ctx

let step_budget : int ref = ref (-1)

let check_step () =
  if !step_budget = 0 then raise StepLimitExceeded
  else if !step_budget > 0 then decr step_budget

let run ?(max_steps = -1) target spec rid values filename =
  step_budget := max_steps;
  Eval_il.step_hook := check_step;
  Eval_il.run target spec rid values filename
