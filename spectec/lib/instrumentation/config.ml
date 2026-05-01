type t = Instrumentation_core.Config.t list

let default = []

let register_static_dependencies (config : t) =
  List.iter Instrumentation_core.Config.register_static_dependencies config

let handlers (config : t) =
  List.map Instrumentation_core.Config.to_handler config

let validate_mode (config : t) ~sl_mode =
  let interp_mode = if sl_mode then `SL else `IL in
  let incompatible =
    List.filter_map
      (fun ({ Instrumentation_core.Config.name; mode; _ } :
             Instrumentation_core.Config.t) ->
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
  List.iter Instrumentation_core.Config.close_output config
