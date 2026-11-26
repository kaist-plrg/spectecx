module Location = Common.Location

let position session (frame : Engine.frame) =
  match Option.value frame.premise ~default:frame.location with
  | Location.Span { file; left; right } ->
      let line = Session.client_line session
      and column = Session.client_column session in
      [
        ( "source",
          `Assoc
            [
              ("name", `String (Filename.basename file));
              ("path", `String (Engine.canonical file));
            ] );
        ("line", line left);
        ("column", column left);
        ("endLine", line right);
        ("endColumn", column right);
      ]
  | Location.Unknown | Location.File _ ->
      [ ("line", `Int 0); ("column", `Int 0) ]

let stopped : Engine.stop_reason -> Yojson.Safe.t = function
  | Breakpoints hits ->
      `Assoc
        [
          ("reason", `String "breakpoint");
          ("threadId", `Int Session.thread_id);
          ("allThreadsStopped", `Bool true);
          ( "hitBreakpointIds",
            `List
              (List.map
                 (fun ({ number; _ } : Engine.breakpoint) -> `Int number)
                 hits) );
        ]
  | Stepped _ | Returned _ ->
      `Assoc
        [
          ("reason", `String "step");
          ("threadId", `Int Session.thread_id);
          ("allThreadsStopped", `Bool true);
        ]
