(** Debugger DAP frontend: serves the Debug Adapter Protocol on stdin and
    stdout, driving the engine.

    The client configures the session ([initialize], breakpoints) and starts the
    run with [configurationDone]. Requests are read only while the run is
    stopped or not yet started. The run's own output goes to stderr, since
    stdout carries the protocol.

    Function breakpoints name a relation ([Rel]) or one of its rules
    ([Rel/rule]). Line breakpoints are not supported yet. *)

module Location = Common.Location
module Json = Yojson.Safe
module Util = Yojson.Safe.Util

exception Bad_request of string

let bad_request fmt =
  Format.kasprintf (fun message -> raise (Bad_request message)) fmt

(* The run has a single thread. *)
let thread_id = 1

type session = {
  conn : Transport.t;
  (* What the client counts lines and columns from: 1 unless it says 0. *)
  mutable line_base : int;
  mutable column_base : int;
}

let current : session option ref = ref None

let session () =
  match !current with
  | Some session -> session
  | None -> invalid_arg "Dap: no session"

(* Moves the run's output off stdout, and connects to the client over stdin
   and the original stdout. *)
let connect () =
  Format.pp_print_flush Format.std_formatter ();
  let protocol = Unix.dup Unix.stdout in
  Unix.dup2 Unix.stderr Unix.stdout;
  let conn = Transport.create stdin (Unix.out_channel_of_descr protocol) in
  current := Some { conn; line_base = 1; column_base = 1 }

(* Arguments *)

let member name (request : Transport.request) =
  Util.member name request.arguments

let int_or name ~default request =
  match member name request with
  | `Null -> default
  | `Int n -> n
  | _ -> bad_request "`%s` is not a number." name

let bool_or name ~default request =
  match member name request with
  | `Null -> default
  | `Bool b -> b
  | _ -> bad_request "`%s` is not a boolean." name

(* Initialization *)

let capabilities =
  `Assoc
    [
      ("supportsConfigurationDoneRequest", `Bool true);
      ("supportsFunctionBreakpoints", `Bool true);
      ("supportsTerminateRequest", `Bool true);
    ]

let initialize session request =
  let base name = if bool_or name ~default:true request then 1 else 0 in
  session.line_base <- base "linesStartAt1";
  session.column_base <- base "columnsStartAt1"

(* Breakpoints *)

let target_of_name name : Engine.target option =
  match String.split_on_char '/' (String.trim name) with
  | [ id ] when id <> "" -> Some (Rel id)
  | [ id; rule_id ] when id <> "" && rule_id <> "" -> Some (Rule (id, rule_id))
  | _ -> None

let unverified message =
  `Assoc [ ("verified", `Bool false); ("message", `String message) ]

(* Replaces every relation and rule breakpoint with those named, answering
   for each name in order. *)
let set_function_breakpoints request =
  let names =
    match member "breakpoints" request with
    | `List breakpoints ->
        List.map
          (fun breakpoint -> Util.member "name" breakpoint |> Util.to_string)
          breakpoints
    | _ -> bad_request "`breakpoints` is not a list."
  in
  (* clear all existing relation / rule breakpoints. *)
  Engine.breakpoints ()
  |> List.iter (fun ({ number; target } : Engine.breakpoint) ->
         match target with
         | Rel _ | Rule _ -> Engine.delete_breakpoint number
         | Prem _ -> ());
  let answer name =
    match target_of_name name with
    | None -> unverified ("Cannot read `" ^ name ^ "` as Rel or Rel/rule.")
    | Some target -> (
        match Engine.add_breakpoint target with
        | `New { number; _ } | `Existing { number; _ } ->
            `Assoc [ ("id", `Int number); ("verified", `Bool true) ]
        | exception Engine.Error message -> unverified message)
  in
  `Assoc [ ("breakpoints", `List (List.map answer names)) ]

let set_breakpoints request =
  let lines =
    match member "breakpoints" request with `List lines -> lines | _ -> []
  in
  `Assoc
    [
      ( "breakpoints",
        `List
          (List.map
             (fun _ -> unverified "Line breakpoints are not supported yet.")
             lines) );
    ]

(* Stack frames *)

let absolute file =
  if Filename.is_relative file then Filename.concat (Sys.getcwd ()) file
  else file

(* Where a frame is shown: its current premise, or else its rule, clause or
   definition. *)
let position session (frame : Engine.frame) =
  match Option.value frame.premise ~default:frame.location with
  | Location.Span { file; left; right } ->
      let line (pos : Location.pos) = `Int (pos.line - 1 + session.line_base) in
      let column (pos : Location.pos) =
        `Int (pos.column + session.column_base)
      in
      [
        ( "source",
          `Assoc
            [
              ("name", `String (Filename.basename file));
              ("path", `String (absolute file));
            ] );
        ("line", line left);
        ("column", column left);
        ("endLine", line right);
        ("endColumn", column right);
      ]
  | Location.Unknown | Location.File _ ->
      [ ("line", `Int 0); ("column", `Int 0) ]

let stack_trace session request =
  let frames = Engine.frames () in
  let start = max 0 (int_or "startFrame" ~default:0 request) in
  let levels = int_or "levels" ~default:0 request in
  let shown =
    List.filteri
      (fun index _ -> index >= start && (levels <= 0 || index < start + levels))
      frames
  in
  `Assoc
    [
      ( "stackFrames",
        `List
          (List.mapi
             (fun offset (frame : Engine.frame) ->
               `Assoc
                 (("id", `Int (start + offset))
                 :: ("name", `String frame.name)
                 :: position session frame))
             shown) );
      ("totalFrames", `Int (List.length frames));
    ]

(* Requests *)

let quit () = exit 1

(* Answers [request], and returns how to go on when it resumes the run. Before
   the run, [running] is false and [configurationDone] starts it. *)
let handle session ~running (request : Transport.request) : Engine.resume option
    =
  let respond ?body () = Transport.respond session.conn ?body request in
  let require_running () =
    if not running then bad_request "The run hasn't started."
  in
  let resume (resume : Engine.resume) =
    require_running ();
    respond ();
    Some resume
  in
  match request.command with
  | "initialize" ->
      initialize session request;
      respond ~body:capabilities ();
      Transport.send_event session.conn "initialized";
      None
  | "launch" ->
      respond ();
      None
  | "setFunctionBreakpoints" ->
      respond ~body:(set_function_breakpoints request) ();
      None
  | "setBreakpoints" ->
      respond ~body:(set_breakpoints request) ();
      None
  | "setExceptionBreakpoints" ->
      respond ();
      None
  | "configurationDone" ->
      if running then bad_request "The run is already in progress.";
      respond ();
      Some Continue
  | "threads" ->
      respond
        ~body:
          (`Assoc
             [
               ( "threads",
                 `List
                   [
                     `Assoc [ ("id", `Int thread_id); ("name", `String "main") ];
                   ] );
             ])
        ();
      None
  | "stackTrace" ->
      require_running ();
      respond ~body:(stack_trace session request) ();
      None
  | "scopes" ->
      require_running ();
      respond ~body:(`Assoc [ ("scopes", `List []) ]) ();
      None
  | "continue" -> resume Continue
  | "next" -> resume Next
  | "stepIn" -> resume Step
  | "stepOut" -> resume Finish
  | "disconnect" | "terminate" ->
      respond ();
      quit ()
  | command -> bad_request "Unsupported request `%s`." command

(* Serves requests until one starts or resumes the run. *)
let rec serve ~running : Engine.resume =
  let session = session () in
  match Transport.read_request session.conn with
  | None -> quit ()
  | Some request -> (
      let fail message = Transport.respond_error session.conn request message in
      match handle session ~running request with
      | Some resume -> resume
      | None -> serve ~running
      | exception (Bad_request message | Engine.Error message) ->
          fail message;
          serve ~running
      | exception Util.Type_error (message, _) ->
          fail ("Bad arguments: " ^ message);
          serve ~running)
  | exception Transport.Protocol_error message ->
      prerr_endline ("DAP: " ^ message);
      quit ()

(* Stops *)

let stopped_body : Engine.stop_reason -> Json.t = function
  | Breakpoints hits ->
      `Assoc
        [
          ("reason", `String "breakpoint");
          ("threadId", `Int thread_id);
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
          ("threadId", `Int thread_id);
          ("allThreadsStopped", `Bool true);
        ]

module Frontend = struct
  let before_run () =
    connect ();
    ignore (serve ~running:false)

  let stopped reason =
    Transport.send_event (session ()).conn "stopped" ~body:(stopped_body reason);
    serve ~running:true

  let after_run () = Transport.send_event (session ()).conn "terminated"
end

module Handler = Engine.Make (Frontend)

let config : Instrumentation_config.Handler_config.t =
  {
    name = "debugger";
    modes = [ `AL ];
    handler = (module Handler : Instrumentation_api.Handler.S);
    output = Instrumentation_api.Output.stdout;
  }
