(** Debugger DAP frontend: serves the Debug Adapter Protocol on stdin and
    stdout, driving the engine.

    The client configures the session ([initialize], breakpoints) and starts the
    run with [configurationDone]. Requests are read only while the run is
    stopped or not yet started. The run's own output goes to stderr, since
    stdout carries the protocol.

    Function breakpoints name a relation ([Rel]) or one of its rules
    ([Rel/rule]). Line breakpoints are on written premises, set per source file;
    paths are compared and reported in canonical form ({!Engine.canonical}). *)

module Location = Common.Location
module Il = Lang.Il
module VEnv = Envs_interp.VEnv
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
      ("supportsEvaluateForHovers", `Bool true);
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

(* Lines and columns as the client counts them. The engine counts lines from 1
   and columns from 0. *)
let client_line session (pos : Location.pos) =
  `Int (pos.line - 1 + session.line_base)

let client_column session (pos : Location.pos) =
  `Int (pos.column + session.column_base)

(* Replaces the premise breakpoints in one source with those on the given
   lines, answering for each line in order. A line between premises moves to
   the next one, and the answer says where it landed. *)
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
    match Engine.premise_at ~file:path ~line:(line - session.line_base + 1) with
    | exception Engine.Error message -> unverified message
    | site -> (
        let landed =
          match site.span with
          | Location.Span { left; _ } -> [ ("line", client_line session left) ]
          | Location.Unknown | Location.File _ -> []
        in
        match Engine.add_breakpoint (Prem site) with
        | `New { number; _ } | `Existing { number; _ } ->
            `Assoc (("id", `Int number) :: ("verified", `Bool true) :: landed))
  in
  `Assoc [ ("breakpoints", `List (List.map answer lines)) ]

(* Stack frames *)

(* Where a frame is shown: its current premise, or else its rule, clause or
   definition. *)
let position session (frame : Engine.frame) =
  match Option.value frame.premise ~default:frame.location with
  | Location.Span { file; left; right } ->
      let line = client_line session and column = client_column session in
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

(* Variables *)

(* What a variablesReference stands for: a scope's entries, or a value whose
   parts the client can expand. References are handed out as the client asks,
   and last until the run resumes. *)
type container = Entries of (string * Il.Value.t) list | Parts of Il.Value.t

let containers : (int, container) Hashtbl.t = Hashtbl.create 16

let new_reference container =
  let reference = Hashtbl.length containers + 1 in
  Hashtbl.replace containers reference container;
  reference

(* The parts of a value, named as the client shows them. *)
let parts (value : Il.Value.t) =
  let indexed values = List.mapi (fun i v -> (string_of_int i, v)) values in
  match value.it with
  | StructV fields ->
      List.map (fun (atom, v) -> (Il.Print.string_of_atom atom, v)) fields
  | CaseV case -> indexed (Il.Mixfix.args case)
  | TupleV values | ListV values -> indexed values
  | OptV (Some v) -> indexed [ v ]
  | OptV None | BoolV _ | NumV _ | TextV _ | FuncV _ | OpaqueV _ -> []

(* The value as the CLI prints it, folded onto one line. *)
let one_line (value : Il.Value.t) =
  Il.Print.string_of_value ~parens:true value
  |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (( <> ) "")
  |> String.concat " "

(* [value_key] is "value" in a variable and "result" in an evaluation. *)
let describe ~value_key (value : Il.Value.t) =
  [
    (value_key, `String (one_line value));
    ( "type",
      `String
        (Il.Print.string_of_typ
           Common.Annotated.(value.note.typ $ Location.Unknown)) );
    ( "variablesReference",
      `Int (match parts value with [] -> 0 | _ -> new_reference (Parts value))
    );
  ]

let variable (name, value) =
  `Assoc (("name", `String name) :: describe ~value_key:"value" value)

let frame_at index =
  match List.nth_opt (Engine.frames ()) index with
  | Some frame when index >= 0 -> frame
  | _ | (exception Invalid_argument _) -> bad_request "No frame %d." index

let locals (frame : Engine.frame) =
  VEnv.bindings frame.env
  |> List.map (fun (var, value) -> (Envs_interp.Var.to_string var, value))

let scopes request =
  let frame =
    match member "frameId" request with
    | `Int index -> frame_at index
    | _ -> bad_request "`frameId` is not a number."
  in
  let scope name hint entries =
    `Assoc
      [
        ("name", `String name);
        ("presentationHint", `String hint);
        ( "variablesReference",
          `Int
            (match entries with
            | [] -> 0
            | _ -> new_reference (Entries entries)) );
        ("expensive", `Bool false);
      ]
  in
  (* bound in turn, so that references are handed out in the order shown *)
  let inputs =
    scope "Inputs" "arguments"
      (List.mapi (fun i v -> (string_of_int i, v)) frame.inputs)
  in
  let locals = scope "Locals" "locals" (locals frame) in
  `Assoc [ ("scopes", `List [ inputs; locals ]) ]

let variables request =
  let reference =
    match member "variablesReference" request with
    | `Int reference -> reference
    | _ -> bad_request "`variablesReference` is not a number."
  in
  let entries =
    match Hashtbl.find_opt containers reference with
    | Some (Entries entries) -> entries
    | Some (Parts value) -> parts value
    | None -> bad_request "No variables with reference %d." reference
  in
  `Assoc [ ("variables", `List (List.map variable entries)) ]

(* Evaluates a variable of the frame: [x*] names that binding, and [x] names
   plain [x] or, failing that, the one binding of [x] with iterations. *)
let evaluate request =
  let expression =
    match member "expression" request with
    | `String expression -> String.trim expression
    | _ -> bad_request "`expression` is not a string."
  in
  let frame = frame_at (int_or "frameId" ~default:0 request) in
  let value =
    match List.assoc_opt expression (locals frame) with
    | Some value -> value
    | None -> (
        match
          VEnv.bindings frame.env
          |> List.filter (fun (((id : Il.id), _), _) -> id.it = expression)
        with
        | [ (_, value) ] -> value
        | [] -> bad_request "No local `%s`." expression
        | several ->
            bad_request "`%s` could be %s; give the full name." expression
              (String.concat " or "
                 (List.map
                    (fun (var, _) -> Envs_interp.Var.to_string var)
                    several)))
  in
  `Assoc (describe ~value_key:"result" value)

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
      respond ~body:(set_breakpoints session request) ();
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
      respond ~body:(scopes request) ();
      None
  | "variables" ->
      require_running ();
      respond ~body:(variables request) ();
      None
  | "evaluate" ->
      require_running ();
      respond ~body:(evaluate request) ();
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
    Hashtbl.reset containers;
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
