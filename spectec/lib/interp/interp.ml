module Builtins = Builtins
module Opaque_types = Opaque_types
module Value_typecheck = Value_typecheck
module Target = Target

type target_state = TargetState of (module Target.S)

type error =
  | EvalIlError of Eval_il.error
  | EvalSlError of Eval_sl.error
  | EvalPlError of Eval_pl.error

let error_to_diagnostic = function
  | EvalIlError e -> Eval_il.error_to_diagnostic e
  | EvalSlError e -> Eval_sl.error_to_diagnostic e
  | EvalPlError e -> Eval_pl.error_to_diagnostic e

let with_target_state target f =
  let module T = (val target : Target.S) in
  T.with_state (fun () ->
      let next_vid = ref 0 in
      let fresh_vid () =
        let vid = !next_vid in
        incr next_vid;
        vid
      in
      Lang.Il.Value.GlobalVidProvider.with_provider fresh_vid (fun () ->
          f (TargetState target)))

type context =
  | Il of Eval_il.context
  | Sl of Eval_sl.context
  | Pl of Eval_pl.context

let context_of_il_spec ?(check_determinism = false) (TargetState target) spec =
  Eval_il.context_of_spec ~check_determinism target spec
  |> Result.map (fun context -> Il context)
  |> Result.map_error (fun e -> EvalIlError e)

let context_of_sl_spec ?(check_determinism = false) (TargetState target) spec =
  Eval_sl.context_of_spec ~check_determinism target spec
  |> Result.map (fun context -> Sl context)
  |> Result.map_error (fun e -> EvalSlError e)

let context_of_pl_spec ?(check_determinism = false) (TargetState target) spec =
  Eval_pl.context_of_spec ~check_determinism target spec
  |> Result.map (fun context -> Pl context)
  |> Result.map_error (fun e -> EvalPlError e)

let eval_rel context ~rel ~values =
  match context with
  | Il context ->
      Eval_il.eval_rel context ~rel ~values
      |> Result.map_error (fun e -> EvalIlError e)
  | Sl context ->
      Eval_sl.eval_rel context ~rel ~values
      |> Result.map_error (fun e -> EvalSlError e)
  | Pl context ->
      Eval_pl.eval_rel context ~rel ~values
      |> Result.map_error (fun e -> EvalPlError e)
