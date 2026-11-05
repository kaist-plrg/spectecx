(** Spectec - Entrypoint API facade.

    Provides the core pipeline (parse, elaborate, structure), a unified
    interpreter entry point, and the core type modules (Error, Task, Target). *)

module Error = Error
module Task = Task
module Target = Target
module Interp_mode = Interp_mode
module Diagnostic = Diag

type 'a result = ('a, Error.t) Stdlib.result

let ( let* ) = Result.bind

(* --- Diagnostics --- *)

let with_warnings f =
  Diag.Sink.reset_global ();
  let result = f () in
  let bag = Diag.Sink.drain (Diag.Sink.global ()) in
  (result, bag)

let with_diagnostics f =
  let result, bag = with_warnings f in
  let bag =
    match result with
    | Ok _ -> bag
    | Error e -> Diag.Bag.merge bag (Error.to_diagnostics e)
  in
  (result, bag)

(* --- Spec membership --- *)

let spec_root_of_file file = Spec_files.root_of_file file
let collect_spec_files dir = Spec_files.collect dir
let collect_spec_siblings dir = Spec_files.collect ~recursive:false dir
let compare_spec_files = Spec_files.compare_paths

(* --- Pipeline transformations --- *)

type spec_source = Pass.spec_source = { filename : string; contents : string }

let parse_spec_files filenames =
  Pass.parse_files filenames |> Result.map_error (fun e -> Error.PassError e)

let parse_spec_source source =
  Pass.parse_source source |> Result.map_error (fun e -> Error.PassError e)

let parse_spec_sources sources =
  Pass.parse_sources sources |> Result.map_error (fun e -> Error.PassError e)

let elaborate spec_el =
  Pass.elaborate spec_el |> Result.map_error (fun e -> Error.PassError e)

(* Structuring depends only on the spec, so caching on its identity is sound. *)
let structure =
  let cache = ref None in
  fun spec_il ->
    match !cache with
    | Some (spec_il', spec_sl) when spec_il' == spec_il -> spec_sl
    | _ ->
        let spec_sl = Pass.structure spec_il in
        cache := Some (spec_il, spec_sl);
        spec_sl

let henv_of_el_spec spec_el = Pass.henv_of_el_spec spec_el
let henv_with_il_spec henv spec_il = Pass.henv_with_il_spec henv spec_il
let annotate ~henv spec_sl = Pass.annotate ~henv spec_sl
let shorten spec_pl = Pass.shorten spec_pl

let validate_config config ~(mode : Interp_mode.request) =
  Instrumentation.Config.validate_mode config ~mode
  |> Result.map_error (fun msg ->
         Error.ConfigError (Common.Location.Unknown, msg))

(* --- Unified interpreter entry point --- *)

let interp_error e = Error.InterpError e

let eval_run ~check_determinism ~(mode : Interp_mode.t) ~spec_il target_state
    (run : Task.run) =
  let driver =
    match run with
    | Relation (relation, values) -> fun ~eval_rel -> eval_rel relation values
    | Driver drive -> drive
  in
  let run_layer context =
    let* context = Result.map_error interp_error context in
    driver ~eval_rel:(fun rel values ->
        Interp.eval_rel context ~rel ~values |> Result.map_error interp_error)
  in
  match mode with
  | Il ->
      run_layer
        (Interp.context_of_il_spec ~check_determinism target_state spec_il)
  | Sl ->
      let spec_sl = structure spec_il in
      run_layer
        (Interp.context_of_sl_spec ~check_determinism target_state spec_sl)
  | Pl henv ->
      let spec_sl = structure spec_il in
      let spec_pl = Pass.annotate ~henv spec_sl |> Pass.shorten in
      run_layer
        (Interp.context_of_pl_spec ~check_determinism target_state spec_pl)

let eval_task (type i) (module T : Task.S with type input = i)
    ?(check_determinism = false) ~(mode : Interp_mode.t) ~spec_il (input : i) =
  Interp.with_target_state (module T.Target) @@ fun target_state ->
  let* run = T.parse_input ~spec:spec_il input in
  eval_run ~check_determinism ~mode ~spec_il target_state run

let eval_task_with_instrumentation (type i)
    (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ?(check_determinism = false)
    ~(mode : Interp_mode.t) ~spec_il (input : i) =
  let instrumented_spec =
    match mode with
    | Il -> Instrumentation.Static.IlSpec spec_il
    | Sl | Pl _ -> Instrumentation.Static.SlSpec (structure spec_il)
  in
  Instrumentation.with_instrumentation config instrumented_spec @@ fun () ->
  eval_task (module T) ~check_determinism ~mode ~spec_il input
