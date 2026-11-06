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
  mutable clause_idx : int option;
  (* A rule breakpoint waiting for its rule's guards, listed here, to pass. *)
  mutable pending : (breakpoint * Il.prem list) option;
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

(* Source *)

(* The definition of the relation or function a frame runs. *)
let find_def frame =
  List.find_opt
    (fun (def : Il.def) ->
      match (frame.kind, def.it) with
      | `Rel, Il.RelD { relid; _ } -> relid.it = frame.id
      | `Func, (Il.DecD { defid; _ } | Il.BuiltinDecD { defid; _ }) ->
          defid.it = frame.id
      | _ -> false)
    !State.spec

(* Where a frame is in the spec: its current rule or clause, or else its
   definition. *)
let location_of_frame frame =
  match find_def frame with
  | None -> Location.Unknown
  | Some def -> (
      let inner =
        match (def.it, frame.rule_id, frame.clause_idx) with
        | Il.RelD { rules; _ }, Some rule_id, _ ->
            List.find_opt
              (fun (rule : Il.rule) -> rule.it.ruleid.it = rule_id)
              rules
            |> Option.map (fun (rule : Il.rule) -> rule.at)
        | Il.DecD { clauses; _ }, _, Some idx ->
            List.nth_opt clauses idx
            |> Option.map (fun (clause : Il.clause) -> clause.at)
        | _ -> None
      in
      match inner with Some at -> at | None -> def.at)

let lines_of_file file =
  match In_channel.with_open_text file In_channel.input_all with
  | text -> Some (String.split_on_char '\n' text |> Array.of_list)
  | exception Sys_error _ -> None

(* Lines shown around a span, and the most of a long span shown. *)
let context_lines = 2
let max_span_lines = 20

let print_source at =
  match at with
  | Location.Unknown -> say "No source location."
  | Location.File file -> say "No line information in %s." file
  | Location.Span { file; left; right } -> (
      match lines_of_file file with
      | None -> say "Cannot read %s." file
      | Some lines ->
          let truncated = right.line - left.line + 1 > max_span_lines in
          let first = max 1 (left.line - context_lines) in
          let last =
            if truncated then left.line + max_span_lines - 1
            else right.line + context_lines
          in
          let last = min (Array.length lines) last in
          say "%s" (Location.to_string at);
          Array.sub lines (first - 1) (last - first + 1)
          |> Array.iteri (fun offset text ->
                 let line = first + offset in
                 let marker =
                   if line >= left.line && line <= right.line then ">" else " "
                 in
                 say "%s%5d  %s" marker line text);
          if truncated then say "   ...")

let print_current_source () =
  match !State.frames with
  | [] -> say "No call in progress."
  | frame :: _ -> print_source (location_of_frame frame)

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
  show               show the source of the current rule or clause
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
      | [ "show" ] ->
          require_running ();
          print_current_source ();
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

(* Rule breakpoints. A rule is tried before it is known to apply: its
   conclusion's input patterns are checked by premises that elaboration puts
   first and marks [Synthesized]. A rule breakpoint stops once those pass. *)

let find_rule id rule_id =
  List.find_map
    (fun (def : Il.def) ->
      match def.it with
      | Il.RelD { relid; rules; _ } when relid.it = id ->
          List.find_opt
            (fun (rule : Il.rule) -> rule.it.ruleid.it = rule_id)
            rules
      | _ -> None)
    !State.spec

(* The premises checking the rule's input patterns, with the premises iterated
   inside them, which report their own events. *)
let guard_prems (rule : Il.rule) =
  let rec with_inner (prem : Il.prem) =
    match prem.it with
    | Il.IterPr (inner, _) -> prem :: with_inner inner
    | _ -> [ prem ]
  in
  let rec leading = function
    | (({ note = Some Il.Synthesized; _ } : Il.prem) as prem) :: prems ->
        with_inner prem @ leading prems
    | _ -> []
  in
  leading rule.it.prems

let enter_rule id rule_id =
  match State.breakpoint_for (Rule (id, rule_id)) with
  | None -> ()
  | Some breakpoint -> (
      let guards =
        match find_rule id rule_id with
        | Some rule -> guard_prems rule
        | None -> []
      in
      match (guards, !State.frames) with
      | [], _ | _, [] -> stop_at breakpoint
      | guards, frame :: _ -> frame.pending <- Some (breakpoint, guards))

(* Stops a pending rule breakpoint, when [applied] given the guards. *)
let resolve_pending applied =
  match !State.frames with
  | ({ pending = Some (breakpoint, guards); _ } as frame) :: _
    when applied guards ->
      frame.pending <- None;
      stop_at breakpoint
  | _ -> ()

(* The rule applies once it reaches a premise past its guards. *)
let enter_prem prem =
  resolve_pending (fun guards -> not (List.memq prem guards))

(* A rule whose premises are all guards applies when it succeeds. *)
let exit_rule success =
  resolve_pending (fun _ -> success);
  match !State.frames with frame :: _ -> frame.pending <- None | [] -> ()

let push_frame kind id at inputs =
  State.frames :=
    { kind; id; at; inputs; rule_id = None; clause_idx = None; pending = None }
    :: !State.frames

let pop_frame () =
  match !State.frames with _ :: frames -> State.frames := frames | [] -> ()

let set_rule_id rule_id =
  match !State.frames with frame :: _ -> frame.rule_id <- rule_id | [] -> ()

let set_clause_idx clause_idx =
  match !State.frames with
  | frame :: _ -> frame.clause_idx <- clause_idx
  | [] -> ()

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
        enter_rule id rule_id
    | Rule_exit { success; _ } ->
        exit_rule success;
        set_rule_id None
    | Prem_enter { prem; _ } | Iter_prem_enter { prem; _ } -> enter_prem prem
    | Func_enter { id; at; inputs } -> push_frame `Func id at inputs
    | Clause_enter { clause_idx; _ } -> set_clause_idx (Some clause_idx)
    | Clause_exit _ -> set_clause_idx None
    | Test_start _ | Test_end _ | Iter_prem_exit _ | Prem_exit _ | Instr _ -> ()
end

let config : Instrumentation_config.Handler_config.t =
  {
    name = "debugger";
    modes = [ `IL ];
    handler = (module M : Instrumentation_api.Handler.S);
    output = Instrumentation_api.Output.stdout;
  }
