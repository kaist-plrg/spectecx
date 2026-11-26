module Util = Yojson.Safe.Util
open Request

let quit () = exit 1

(* Answers [request], and returns how to go on when it resumes the run. Before
   the run, [running] is false and [configurationDone] starts it. *)
let handle (session : Session.t) ~running (request : Transport.request) :
    Engine.resume option =
  let respond ?body () = Transport.respond session.conn ?body request in
  let require_running () =
    if not running then bad_request "The run hasn't started."
  in
  let resume (resume : Engine.resume) =
    require_running ();
    respond ();
    Some resume
  in
  let inspect body =
    require_running ();
    respond ~body:(body ()) ();
    None
  in
  match request.command with
  | "initialize" ->
      Configure.initialize session request;
      respond ~body:Configure.capabilities ();
      Transport.send_event session.conn "initialized";
      None
  | "launch" ->
      respond ();
      None
  | "setFunctionBreakpoints" ->
      respond ~body:(Configure.set_function_breakpoints request) ();
      None
  | "setBreakpoints" ->
      respond ~body:(Configure.set_breakpoints session request) ();
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
                     `Assoc
                       [
                         ("id", `Int Session.thread_id); ("name", `String "main");
                       ];
                   ] );
             ])
        ();
      None
  | "stackTrace" -> inspect (fun () -> Inspect.stack_trace session request)
  | "scopes" -> inspect (fun () -> Inspect.scopes request)
  | "variables" -> inspect (fun () -> Inspect.variables request)
  | "evaluate" -> inspect (fun () -> Inspect.evaluate request)
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
  let session = Session.current () in
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

let before_run () =
  Session.connect ();
  ignore (serve ~running:false)

let stopped reason =
  Inspect.reset ();
  Transport.send_event (Session.current ()).conn "stopped"
    ~body:(Convert.stopped reason);
  serve ~running:true

let after_run () = Transport.send_event (Session.current ()).conn "terminated"

let config : Instrumentation_config.Handler_config.t =
  {
    name = "debugger";
    modes = [ `AL ];
    handler = Engine.handler ~before_run ~stopped ~after_run;
    output = Instrumentation_api.Output.stdout;
  }
