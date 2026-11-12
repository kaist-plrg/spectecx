(** Debugger: drives an IL run from commands read on stdin.

    The prompt opens before the run starts, so breakpoints can be set and the
    run started with [run]. Breakpoints name a relation ([Rel]) or one of its
    rules ([Rel/rule]). Stepping stops at the premises written in the spec,
    skipping those the algo pass synthesized. Enabled by the [debug] subcommand,
    which also restricts it to single IL runs. *)

module Al = Lang.Al
module Il = Lang.Il
module Location = Common.Location
module VEnv = Envs_interp.VEnv
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
  (* A breakpoint on the current rule that has not fired in this attempt. *)
  mutable armed : breakpoint option;
  (* The bindings in scope, as of the last event in this call. *)
  mutable env : VEnv.t;
  (* The written premise this call is at. *)
  mutable prem_at : Location.t option;
}

(* Where a step stops, by call depth: [Step] at the next premise anywhere,
   [Next] at the next one in a call no deeper, and [Finish] when the call at
   that depth returns. *)
type step = Step | Next of int | Finish of int

let string_of_target = function
  | Rel id -> id
  | Rule (id, rule_id) -> id ^ "/" ^ rule_id

let target_of_string name =
  match String.split_on_char '/' (String.trim name) with
  | [ id ] when id <> "" -> Some (Rel id)
  | [ id; rule_id ] when id <> "" && rule_id <> "" -> Some (Rule (id, rule_id))
  | _ -> None

let defines (spec : Al.spec) target =
  List.exists
    (fun (def : Al.def) ->
      match (def.it, target) with
      | Al.RelD { relid; _ }, Rel id -> relid.it = id
      | Al.RelD { relid; rules; _ }, Rule (id, rule_id) ->
          relid.it = id
          && List.exists
               (fun (rule : Al.rule) -> rule.it.ruleid.it = rule_id)
               rules
      | _ -> false)
    spec

module State = struct
  let spec : Al.spec ref = ref []
  let breakpoints : breakpoint list ref = ref []
  let next_number = ref 1
  let frames : frame list ref = ref []
  let running = ref false
  let step : step option ref = ref None

  let reset () =
    breakpoints := [];
    next_number := 1;
    frames := [];
    running := false;
    step := None

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

(* Prints [value] after [label], aligning its continuation lines. *)
let say_value label value =
  let continuation = "\n" ^ String.make (String.length label) ' ' in
  Il.Print.string_of_value ~parens:true value
  |> String.split_on_char '\n' |> String.concat continuation |> say "%s%s" label

let say_values values =
  List.iteri
    (fun index value -> say_value (Format.sprintf "  %d: " index) value)
    values

let print_args () =
  match !State.frames with
  | [] -> say "No call in progress."
  | frame :: _ when frame.inputs = [] ->
      say "%s takes no inputs." (string_of_frame frame)
  | frame :: _ -> say_values frame.inputs

let say_binding (var, value) =
  say_value ("  " ^ Envs_interp.Var.to_string var ^ " = ") value

let print_locals () =
  match !State.frames with
  | [] -> say "No call in progress."
  | frame :: _ when VEnv.is_empty frame.env -> say "No local bindings."
  | frame :: _ ->
      VEnv.iter (fun var value -> say_binding (var, value)) frame.env

(* Prints the bindings of [name], one per iteration depth. *)
let print_local name =
  match !State.frames with
  | [] -> say "No call in progress."
  | frame :: _ -> (
      match
        VEnv.bindings frame.env
        |> List.filter (fun (((id : Il.id), _), _) -> id.it = name)
      with
      | [] -> raise (Bad_command ("No local `" ^ name ^ "`."))
      | bindings -> List.iter say_binding bindings)

(* Source *)

(* The definition of the relation or function a frame runs. *)
let find_def frame =
  List.find_opt
    (fun (def : Al.def) ->
      match (frame.kind, def.it) with
      | `Rel, Al.RelD { relid; _ } -> relid.it = frame.id
      | `Func, (Al.DecD { defid; _ } | Al.BuiltinDecD { defid; _ }) ->
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
        | Al.RelD { rules; _ }, Some rule_id, _ ->
            List.find_opt
              (fun (rule : Al.rule) -> rule.it.ruleid.it = rule_id)
              rules
            |> Option.map (fun (rule : Al.rule) -> rule.at)
        | Al.DecD { clauses; _ }, _, Some idx ->
            List.nth_opt clauses idx
            |> Option.map (fun (clause : Al.clause) -> clause.at)
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

(* Marks [at]'s lines with [>], and [current]'s with [=>]. *)
let print_source ?current at =
  let within line = function
    | Location.Span { left; right; _ } ->
        line >= left.line && line <= right.line
    | Location.Unknown | Location.File _ -> false
  in
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
                   if Option.fold ~none:false ~some:(within line) current then
                     "=>"
                   else if within line at then " >"
                   else "  "
                 in
                 say "%s%5d  %s" marker line text);
          if truncated then say "   ...")

let print_current_source () =
  match !State.frames with
  | [] -> say "No call in progress."
  | frame :: _ -> print_source ?current:frame.prem_at (location_of_frame frame)

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
  s, step            run to the next premise, entering calls
  n, next            run to the next premise, stepping over calls
  f, finish          run until the current call returns
  bt, backtrace      show the call stack, innermost call first
  args               show the current call's input values
  locals             show the bindings in scope
  p, print NAME      show the binding of NAME
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

let depth () = List.length !State.frames

(* Resumes the run, stepping as [step]. *)
let resume_stepping step =
  require_running ();
  if !State.frames = [] then raise (Bad_command "No call in progress.");
  State.step := Some (step (depth ()));
  true

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
          State.step := None;
          true
      | [ ("s" | "step") ] -> resume_stepping (fun _ -> Step)
      | [ ("n" | "next") ] -> resume_stepping (fun depth -> Next depth)
      | [ ("f" | "finish") ] -> resume_stepping (fun depth -> Finish depth)
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
      | [ "locals" ] ->
          require_running ();
          print_locals ();
          false
      | [ ("p" | "print"); name ] ->
          require_running ();
          print_local name;
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

(* Every stop ends the step in progress. *)
let pause () =
  State.step := None;
  prompt ()

let stop_at { number; target } =
  say "Breakpoint %d, %s%s" number (string_of_target target)
    (current_call_site ());
  pause ()

(* Stepping *)

(* Prints the first line of [at], as gdb does when stepping. *)
let say_line frame at =
  match at with
  | Location.Span { file; left; _ } -> (
      say "%s, %s:%d" (string_of_frame frame) file left.line;
      match lines_of_file file with
      | Some lines when left.line <= Array.length lines ->
          say "%7d  %s" left.line lines.(left.line - 1)
      | _ -> ())
  | Location.Unknown | Location.File _ -> say "%s" (string_of_frame frame)

(* Stops at a written premise when the step in progress lands on it. *)
let step_at at =
  match (!State.frames, !State.step) with
  | frame :: _, Some Step ->
      say_line frame at;
      pause ()
  | frame :: _, Some (Next depth') when depth () <= depth' ->
      say_line frame at;
      pause ()
  | _ -> ()

(* Pops the innermost call, stopping when it is the one being finished. *)
let leave_call describe_result =
  let finishing =
    match !State.step with
    | Some (Finish depth') -> depth () = depth'
    | _ -> false
  in
  let name =
    match !State.frames with frame :: _ -> string_of_frame frame | [] -> ""
  in
  (match !State.frames with _ :: frames -> State.frames := frames | [] -> ());
  if finishing then (
    describe_result name;
    pause ())

let describe_rel_exit success conclusion name =
  if not success then say "%s failed." name
  else
    match Il.Mode.outputs conclusion |> List.filter_map Fun.id with
    | [] -> say "%s holds." name
    | outputs ->
        say "%s returned:" name;
        say_values outputs

let describe_func_exit output name =
  match output with
  | None -> say "%s failed." name
  | Some value -> say_value (name ^ " returned ") value

let stop_if_breakpoint target =
  Option.iter stop_at (State.breakpoint_for target)

(* Rule breakpoints. A rule is tried before it is known to apply: its
   conclusion's input patterns are checked by instructions that the algo pass
   puts first, with origin [Input_sidecondition]. Every instruction after them
   starts with a written premise, so a rule breakpoint is armed when the rule is
   entered and fires at its first written premise, or when the rule succeeds if
   it has none. *)

let arm_rule id rule_id =
  match (State.breakpoint_for (Rule (id, rule_id)), !State.frames) with
  | Some breakpoint, frame :: _ -> frame.armed <- Some breakpoint
  | _ -> ()

(* Stops at the armed breakpoint, if any, and says whether it stopped. *)
let fire_armed () =
  match !State.frames with
  | ({ armed = Some breakpoint; _ } as frame) :: _ ->
      frame.armed <- None;
      stop_at breakpoint;
      true
  | _ -> false

(* A rule failing a guard never reached a written premise, so does not stop. *)
let exit_rule success =
  if success then ignore (fire_armed ());
  match !State.frames with frame :: _ -> frame.armed <- None | [] -> ()

let push_frame kind id at inputs =
  State.frames :=
    {
      kind;
      id;
      at;
      inputs;
      rule_id = None;
      clause_idx = None;
      armed = None;
      env = VEnv.empty;
      prem_at = None;
    }
    :: !State.frames

let set_rule_id rule_id =
  match !State.frames with frame :: _ -> frame.rule_id <- rule_id | [] -> ()

let set_clause_idx clause_idx =
  match !State.frames with
  | frame :: _ -> frame.clause_idx <- clause_idx
  | [] -> ()

let set_env env =
  match !State.frames with frame :: _ -> frame.env <- env | [] -> ()

let set_prem_at at =
  match !State.frames with frame :: _ -> frame.prem_at <- Some at | [] -> ()

(* A rule or clause ended: nothing from it stays in scope. *)
let leave_scope () =
  match !State.frames with
  | frame :: _ ->
      frame.env <- VEnv.empty;
      frame.prem_at <- None
  | [] -> ()

(* An instruction the spec's author wrote as a premise, not one the algo pass
   synthesized. *)
let is_written (instr : Al.instr) =
  match instr.note with
  | Al.Premise _ -> true
  | Al.Sidecondition _ | Al.Input_sidecondition -> false

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []
  let handle_value = None

  let init ~(spec : Instrumentation_api.Handler.spec) =
    State.reset ();
    (match spec with
    | AlSpec al_spec -> State.spec := al_spec
    | SlSpec _ -> invalid_arg "Debugger.init: the debug subcommand runs AL");
    say "Set breakpoints, then `run` to start. `help` lists commands.";
    prompt ()

  let finish () = ()

  let handle : Instrumentation_api.Event.t -> unit = function
    | Rel_enter { id; at; inputs } ->
        push_frame `Rel id at inputs;
        stop_if_breakpoint (Rel id)
    | Rel_exit { success; conclusion; _ } ->
        leave_call (describe_rel_exit success conclusion)
    | Func_exit { output; _ } -> leave_call (describe_func_exit output)
    | Rule_enter { id; rule_id; at = _; env } ->
        set_rule_id (Some rule_id);
        set_env env;
        arm_rule id rule_id
    | Rule_exit { success; _ } ->
        exit_rule success;
        set_rule_id None;
        leave_scope ()
    | Instr_enter { instr = AlInstr instr; at; env } ->
        set_env env;
        if is_written instr then (
          set_prem_at at;
          (* One stop per event: a rule breakpoint firing here ends any step. *)
          if not (fire_armed ()) then step_at at)
    | Instr_exit { instr = AlInstr _; env; _ } -> set_env env
    | Func_enter { id; at; inputs } -> push_frame `Func id at inputs
    | Clause_enter { clause_idx; env; _ } ->
        set_clause_idx (Some clause_idx);
        set_env env
    | Clause_exit _ ->
        set_clause_idx None;
        leave_scope ()
    | Test_start _ | Test_end _ | Repetition_enter _ | Repetition_exit _
    | Case_match _ | Instr_enter _ | Instr_exit _ ->
        ()
end

let config : Instrumentation_config.Handler_config.t =
  {
    name = "debugger";
    modes = [ `AL ];
    handler = (module M : Instrumentation_api.Handler.S);
    output = Instrumentation_api.Output.stdout;
  }
