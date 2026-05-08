module Builtins = Builtins
module Target = Target

type error = EvalIlError of Eval_il.error | EvalSlError of Eval_sl.error
type ctx_il = Eval_il.Ctx.t
type ctx_sl = Eval_sl.Ctx.t

exception StepLimitExceeded = Eval_il.StepLimitExceeded

let error_to_string = function
  | EvalIlError e -> Eval_il.error_to_string e
  | EvalSlError e -> Eval_sl.error_to_string e

let error_to_diagnostic = function
  | EvalIlError e -> Eval_il.error_to_diagnostic e
  | EvalSlError e -> Eval_sl.error_to_diagnostic e

let eval_il ?(max_steps = -1) target spec rid args filename =
  Eval_il.run ~max_steps target spec rid args filename
  |> Result.map_error (fun e -> EvalIlError e)

let eval_sl target spec rid args filename =
  Eval_sl.run target spec rid args filename
  |> Result.map_error (fun e -> EvalSlError e)
