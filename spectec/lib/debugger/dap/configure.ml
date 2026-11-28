module Location = Common.Location
module Util = Yojson.Safe.Util
open Request

let capabilities =
  `Assoc
    [
      ("supportsConfigurationDoneRequest", `Bool true);
      ("supportsFunctionBreakpoints", `Bool true);
      ("supportsTerminateRequest", `Bool true);
      ("supportsEvaluateForHovers", `Bool true);
    ]

let initialize (session : Session.t) request =
  let base name = if bool_or name ~default:true request then 1 else 0 in
  session.line_base <- base "linesStartAt1";
  session.column_base <- base "columnsStartAt1"

let unverified message =
  `Assoc [ ("verified", `Bool false); ("message", `String message) ]

let set_function_breakpoints request =
  let names =
    match member "breakpoints" request with
    | `List breakpoints ->
        List.map
          (fun breakpoint -> Util.member "name" breakpoint |> Util.to_string)
          breakpoints
    | _ -> bad_request "`breakpoints` is not a list."
  in
  Engine.breakpoints ()
  |> List.iter (fun ({ number; target } : Engine.breakpoint) ->
         match target with
         | Rel _ | Rule _ -> Engine.delete_breakpoint number
         | Prem _ -> ());
  let answer name =
    match Breakpoints.target_of_string name with
    | None -> unverified ("Cannot read `" ^ name ^ "` as Rel or Rel/rule.")
    | Some target -> (
        match Engine.add_breakpoint target with
        | `New { number; _ } | `Existing { number; _ } ->
            `Assoc [ ("id", `Int number); ("verified", `Bool true) ]
        | exception Engine.Error message -> unverified message)
  in
  `Assoc [ ("breakpoints", `List (List.map answer names)) ]

let set_breakpoints session request =
  let path =
    match member "source" request with
    | `Assoc fields -> (
        match List.assoc_opt "path" fields with
        | Some (`String path) -> path
        | _ -> bad_request "Only sources with a `path` are supported.")
    | _ -> bad_request "`source` is not an object."
  in
  let lines =
    match member "breakpoints" request with
    | `Null -> []
    | `List breakpoints ->
        List.map
          (fun breakpoint -> Util.member "line" breakpoint |> Util.to_int)
          breakpoints
    | _ -> bad_request "`breakpoints` is not a list."
  in
  (* No spec file matching [path] means no breakpoints to replace; each line
     is then answered with why. *)
  let source =
    match Engine.spec_file path with
    | file -> Some file
    | exception Engine.Error _ -> None
  in
  let in_source (site : Engine.site) =
    Option.is_some source && Location.file site.span = source
  in
  Engine.breakpoints ()
  |> List.iter (fun ({ number; target } : Engine.breakpoint) ->
         match target with
         | Prem site when in_source site -> Engine.delete_breakpoint number
         | Rel _ | Rule _ | Prem _ -> ());
  let answer line =
    match
      Engine.premise_at ~file:path ~line:(Session.engine_line session line)
    with
    | exception Engine.Error message -> unverified message
    | site -> (
        let landed =
          match site.span with
          | Location.Span { left; _ } ->
              [ ("line", Session.client_line session left) ]
          | Location.Unknown | Location.File _ -> []
        in
        match Engine.add_breakpoint (Prem site) with
        | `New { number; _ } | `Existing { number; _ } ->
            `Assoc (("id", `Int number) :: ("verified", `Bool true) :: landed))
  in
  `Assoc [ ("breakpoints", `List (List.map answer lines)) ]
