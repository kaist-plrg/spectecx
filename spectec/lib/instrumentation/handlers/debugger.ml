(** Debugger: drives an IL run from commands read on stdin.

    The prompt opens before the run starts, so breakpoints can be set and the
    run started with [run]. Breakpoints name a relation ([Rel]) or one of its
    rules ([Rel/rule]). Enabled by the [debug] subcommand, which also restricts
    it to single IL runs. *)

module Il = Lang.Il
module Location = Common.Location
open Common.Annotated

exception Bad_command of string

type target = Rel of string | Rule of string * string
type breakpoint = { number : int; target : target }

type frame = {
  kind : [ `Rel | `Func ];
  id : string;
  at : Location.t;
  inputs : Il.Value.t list;
  mutable rule_id : string option;
}

let string_of_target = function
  | Rel id -> id
  | Rule (id, rule_id) -> id ^ "/" ^ rule_id

let target_of_string name =
  match String.split_on_char '/' (String.trim name) with
  | [ id ] when id <> "" -> Some (Rel id)
  | [ id; rule_id ] when id <> "" && rule_id <> "" -> Some (Rule (id, rule_id))
  | _ -> None

let defines (spec : Il.spec) target =
  List.exists
    (fun (def : Il.def) ->
      match (def.it, target) with
      | Il.RelD { relid; _ }, Rel id -> relid.it = id
      | Il.RelD { relid; rules; _ }, Rule (id, rule_id) ->
          relid.it = id
          && List.exists
               (fun (rule : Il.rule) -> rule.it.ruleid.it = rule_id)
               rules
      | _ -> false)
    spec

module State = struct
  let spec : Il.spec ref = ref []
  let breakpoints : breakpoint list ref = ref []
  let next_number = ref 1
  let frames : frame list ref = ref []
  let running = ref false

  let reset () =
    breakpoints := [];
    next_number := 1;
    frames := [];
    running := false

  let add_breakpoint target =
    if not (defines !spec target) then
      raise
        (Bad_command
           (Format.sprintf "No definition matches `%s`."
              (string_of_target target)));
    let breakpoint = { number = !next_number; target } in
    incr next_number;
    breakpoints := !breakpoints @ [ breakpoint ];
    breakpoint

  let delete_breakpoint number =
    if not (List.exists (fun b -> b.number = number) !breakpoints) then
      raise (Bad_command (Format.sprintf "No breakpoint number %d." number));
    breakpoints := List.filter (fun b -> b.number <> number) !breakpoints

  let breakpoint_for target =
    List.find_opt (fun b -> b.target = target) !breakpoints
end

let stdout = Format.std_formatter
let say fmt = Format.fprintf stdout (fmt ^^ "\n%!")
let echo_commands = not (Unix.isatty Unix.stdin)

let string_of_frame frame =
  match (frame.kind, frame.rule_id) with
  | `Rel, Some rule_id when rule_id <> "" -> frame.id ^ "/" ^ rule_id
  | `Rel, _ -> frame.id
  | `Func, _ -> "$" ^ frame.id

let describe_call_site at =
  match at with
  | Location.Unknown -> ""
  | at -> " (called at " ^ Location.to_string at ^ ")"

let print_backtrace () =
  List.iteri
    (fun depth frame ->
      say "#%d  %s%s" depth (string_of_frame frame)
        (describe_call_site frame.at))
    !State.frames

let print_args () =
  match !State.frames with
  | [] -> say "No call in progress."
  | frame :: _ when frame.inputs = [] ->
      say "%s takes no inputs." (string_of_frame frame)
  | frame :: _ ->
      List.iteri
        (fun index value ->
          let label = Format.sprintf "  %d: " index in
          let continuation = "\n" ^ String.make (String.length label) ' ' in
          Il.Print.string_of_value value
          |> String.split_on_char '\n' |> String.concat continuation
          |> say "%s%s" label)
        frame.inputs

let print_breakpoints () =
  match !State.breakpoints with
  | [] -> say "No breakpoints."
  | breakpoints ->
      List.iter
        (fun { number; target } ->
          say "  %d  %s" number (string_of_target target))
        breakpoints

let help =
  {|Commands:
  r, run             start the run
  c, continue        resume until the next breakpoint
  bt, backtrace      show the call stack, innermost call first
  args               show the current call's input values
  b, break NAME      stop at NAME: Rel or Rel/rule
  d, delete N        remove breakpoint N
  info b             list breakpoints
  q, quit            abort the run
  h, help            show this help|}

let quit () = exit 1

let read_command () =
  Format.fprintf stdout "(dbg) %!";
  match In_channel.input_line stdin with
  | None ->
      say "";
      quit ()
  | Some line ->
      if echo_commands then say "%s" line;
      String.split_on_char ' ' line |> List.filter (( <> ) "")

let require_running () =
  if not !State.running then raise (Bad_command "The run hasn't started.")

(* Returns once the user starts or resumes the run. *)
let rec prompt () =
  let resume =
    try
      match read_command () with
      | [ ("r" | "run") ] ->
          if !State.running then
            raise (Bad_command "The run is already in progress.");
          State.running := true;
          true
      | [ ("c" | "continue") ] ->
          require_running ();
          true
      | [ ("bt" | "backtrace") ] ->
          require_running ();
          print_backtrace ();
          false
      | [ "args" ] ->
          require_running ();
          print_args ();
          false
      | [ ("b" | "break"); name ] -> (
          match target_of_string name with
          | None ->
              raise
                (Bad_command ("Cannot read `" ^ name ^ "` as Rel or Rel/rule."))
          | Some target ->
              let { number; target } = State.add_breakpoint target in
              say "Breakpoint %d at %s." number (string_of_target target);
              false)
      | [ ("d" | "delete"); number ] -> (
          match int_of_string_opt number with
          | None -> raise (Bad_command ("Not a breakpoint number: " ^ number))
          | Some number ->
              State.delete_breakpoint number;
              false)
      | [ "info"; ("b" | "breakpoints") ] ->
          print_breakpoints ();
          false
      | [ ("q" | "quit") ] -> quit ()
      | [ ("h" | "help") ] ->
          say "%s" help;
          false
      | [] -> false
      | command :: _ ->
          raise (Bad_command ("Unknown command `" ^ command ^ "`. Try `help`."))
    with Bad_command message ->
      say "%s" message;
      false
  in
  if not resume then prompt ()

let current_call_site () =
  match !State.frames with
  | frame :: _ -> describe_call_site frame.at
  | [] -> ""

let stop_at { number; target } =
  say "Breakpoint %d, %s%s" number (string_of_target target)
    (current_call_site ());
  prompt ()

let stop_if_breakpoint target =
  Option.iter stop_at (State.breakpoint_for target)

let push_frame kind id at inputs =
  State.frames := { kind; id; at; inputs; rule_id = None } :: !State.frames

let pop_frame () =
  match !State.frames with _ :: frames -> State.frames := frames | [] -> ()

let set_rule_id rule_id =
  match !State.frames with frame :: _ -> frame.rule_id <- rule_id | [] -> ()

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []

  let init ~(spec : Instrumentation_api.Handler.spec) =
    State.reset ();
    (match spec with
    | IlSpec il_spec -> State.spec := il_spec
    | SlSpec _ -> invalid_arg "Debugger.init: the debug subcommand runs IL");
    say "Set breakpoints, then `run` to start. `help` lists commands.";
    prompt ()

  let finish () = ()

  let handle : Instrumentation_api.Event.t -> unit = function
    | Rel_enter { id; at; inputs } ->
        push_frame `Rel id at inputs;
        stop_if_breakpoint (Rel id)
    | Rel_exit _ | Func_exit _ -> pop_frame ()
    | Rule_enter { id; rule_id; at = _; env = _ } ->
        set_rule_id (Some rule_id);
        stop_if_breakpoint (Rule (id, rule_id))
    | Rule_exit _ -> set_rule_id None
    | Func_enter { id; at; inputs } -> push_frame `Func id at inputs
    | Test_start _ | Test_end _ | Clause_enter _ | Clause_exit _
    | Iter_prem_enter _ | Iter_prem_exit _ | Prem_enter _ | Prem_exit _
    | Instr _ ->
        ()
end

let config : Instrumentation_config.Handler_config.t =
  {
    name = "debugger";
    modes = [ `IL ];
    handler = (module M : Instrumentation_api.Handler.S);
    output = Instrumentation_api.Output.stdout;
  }
