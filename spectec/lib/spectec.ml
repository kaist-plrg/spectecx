(** Spectec - Entrypoint API facade.

    Provides the core pipeline (parse, elaborate, structure), a unified
    interpreter entry point, and the core type modules (Error, Task, Target). *)

module Error = Error
module Task = Task
module Target = Target
module Diagnostic = Diag

type 'a result = ('a, Error.t) Stdlib.result

let ( let* ) = Result.bind

(* --- Diagnostics --- *)

let with_diagnostics f =
  Diag.Sink.reset_global ();
  let result = f () in
  let bag = Diag.Sink.drain (Diag.Sink.global ()) in
  (result, bag)

(* --- Pipeline transformations --- *)

let collect_spec_files spec_dir =
  let rec collect spec_files_rev dir =
    let entries = Sys.readdir dir in
    Array.sort String.compare entries;
    Array.fold_left
      (fun spec_files_rev entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then collect spec_files_rev path
        else if Filename.check_suffix entry ".spectec" then
          path :: spec_files_rev
        else spec_files_rev)
      spec_files_rev entries
  in
  collect [] spec_dir |> List.rev

let parse_spec_files filenames =
  Pass.parse_files filenames |> Result.map_error (fun e -> Error.PassError e)

type il = Pass.il = { lang : Lang.Il.spec; qc : Qc_il.spec }

let elaborate spec_el =
  Pass.elaborate spec_el |> Result.map_error (fun e -> Error.PassError e)

let structure spec_il = Pass.structure spec_il

let validate_config config ~sl_mode =
  Instrumentation.Config.validate_mode config ~sl_mode
  |> Result.map_error (fun msg ->
         Error.ConfigError (Common.Source.no_region, msg))

(* --- Unified interpreter entry point --- *)

let eval_task (type i) (module T : Task.S with type input = i) ~sl_mode ~spec_il
    (input : i) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  T.Target.handler @@ fun () ->
  if sl_mode then
    let spec_sl = Pass.structure spec_il in
    Interp.eval_sl (module T.Target) spec_sl relation values (T.source input)
    |> Result.map snd
    |> Result.map_error (fun e -> Error.InterpError e)
  else
    Interp.eval_il (module T.Target) spec_il relation values (T.source input)
    |> Result.map snd
    |> Result.map_error (fun e -> Error.InterpError e)

let eval_task_with_instrumentation (type i)
    (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il (input : i) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  T.Target.handler @@ fun () ->
  if sl_mode then
    let spec_sl = Pass.structure spec_il in
    Instrumentation.with_instrumentation config
      (Instrumentation.Static.SlSpec spec_sl) (fun () ->
        Interp.eval_sl
          (module T.Target)
          spec_sl relation values (T.source input)
        |> Result.map snd
        |> Result.map_error (fun e -> Error.InterpError e))
  else
    Instrumentation.with_instrumentation config
      (Instrumentation.Static.IlSpec spec_il) (fun () ->
        Interp.eval_il
          (module T.Target)
          spec_il relation values (T.source input)
        |> Result.map snd
        |> Result.map_error (fun e -> Error.InterpError e))
