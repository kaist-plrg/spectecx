open Common.Annotated
module Location = Common.Location
open Lang.Sl
open Error
module F = Format

let invoke_relation (ctx : Ctx.t) (rid : id') (values : value list) : value list
    =
  let id = rid $ Location.Unknown in
  let mode, _, _ = Ctx.find_rel_exn ctx ~id in
  let typs =
    List.map
      (fun (exp : Lang.Sl.exp) -> exp.note.typ $ exp.at)
      (Lang.Il.Mode.inputs mode)
  in
  Ctx.check_inputs_exn ctx ~kind:`Relation ~id ~signature:([], typs) ~targs:[]
    ~values;
  match Interp.invoke_rel ctx id values with
  | Some values -> values
  | None -> error Location.Unknown "relation was not matched"

type error = Location.t * string

let catch (f : unit -> 'a) : ('a, error) result =
  try Ok (f ())
  with InterpError (at, msg) | Interp_runtime.Context.Error (at, msg) ->
    Error (at, msg)

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

let error_to_diagnostic ((at, msg) : error) : Diag.t =
  Diag.error ~source:"sl-interp" at msg
