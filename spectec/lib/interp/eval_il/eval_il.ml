open Lang.Il
module F = Format
open Attempt
open Common.Annotated
module Location = Common.Location

let invoke_relation (ctx : Ctx.t) (rid : id') (values : value list) : value list
    =
  let id = rid $ Location.Unknown in
  let reltyp, _ = Ctx.find_rel_exn ctx ~id in
  Ctx.check_inputs_exn ctx ~kind:`Relation ~id
    ~signature:([], Mode.inputs reltyp.it)
    ~targs:[] ~values;
  Interp.invoke_rel ctx id values |> unwrap_or_error

type error = Plain of Location.t * string | Backtrack of failtrace list

let catch (f : unit -> 'a) : ('a, error) result =
  try Ok (f ()) with
  | Error.InterpError (at, msg) | Interp_runtime.Context.Error (at, msg) ->
      Error (Plain (at, msg))
  | Error.BacktrackError failtraces -> Error (Backtrack failtraces)

type context = Ctx.t

let context_of_spec ?(check_determinism = false) (module T : Target.S)
    (spec : spec) : (context, error) result =
  catch (fun () ->
      let builtins = Builtins.make T.builtins in
      let cache =
        Interp_runtime.Cache.make ~is_impure_func:T.is_impure_func
          ~is_impure_rel:T.is_impure_rel ~state_version:T.state_version
      in
      let opaque_types =
        match Opaque_types.make T.opaque_types with
        | Ok types -> types
        | Error message -> Error.error Location.Unknown message
      in
      Interp.context_of_spec spec ~builtins ~cache ~check_determinism
        ~opaque_types)

let eval_rel (context : context) ~rel ~values =
  catch (fun () -> invoke_relation context rel values)

let error_to_diagnostic = function
  | Plain (at, msg) -> Diag.error ~source:"il-interp" at msg
  | Backtrack failtraces ->
      Diag.of_failtraces ~source:"il-interp" ~fallback:"evaluation failed"
        (prune_failtraces failtraces)
