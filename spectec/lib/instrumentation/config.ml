open Instrumentation_core
open Descriptor

type t = selected_handler list

let default = []

let to_handlers (config : t) =
  let handlers = List.map (fun a -> a.handler) config in
  List.iter
    (fun (module H : Handler.S) ->
      List.iter
        (fun (module M : Instrumentation_static.Static.S) ->
          Instrumentation_static.Static.register (module M))
        H.static_dependencies)
    handlers;
  handlers

let validate_mode (config : t) ~sl_mode =
  let interp_mode = if sl_mode then `SL else `IL in
  let incompatible =
    List.filter_map
      (fun { name; mode; _ } ->
        match (interp_mode, mode) with
        | `IL, `SL -> Some (name, "SL only")
        | `SL, `IL -> Some (name, "IL only")
        | _ -> None)
      config
  in
  match incompatible with
  | [] -> Ok ()
  | errs ->
      let mode_str = if sl_mode then "SL" else "IL" in
      let details =
        String.concat ", "
          (List.map (fun (n, reason) -> Printf.sprintf "%s (%s)" n reason) errs)
      in
      Error
        (Printf.sprintf "Instrumentation handlers incompatible with %s mode: %s"
           mode_str details)

let close_outputs (config : t) =
  List.iter (fun a -> Output.close a.output) config
