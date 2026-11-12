(** Debugger: drives an IL run from commands read on stdin.

    The prompt opens before the run starts, so breakpoints can be set and the
    run started with [run]. Breakpoints name a relation ([Rel]), one of its
    rules ([Rel/rule]), or a written premise of a rule or function clause by its
    line ([FILE:LINE]). Stepping stops at the premises written in the spec,
    skipping those the algo pass synthesized. Enabled by the [debug] subcommand,
    which also restricts it to single IL runs. *)

module Al = Lang.Al
module Il = Lang.Il
module Location = Common.Location
module VEnv = Envs_interp.VEnv
open Common.Annotated

exception Bad_command of string

(* A written premise of a rule or clause. [span] is the premise as written;
   [hit] is where it is entered, which is inside [span] for an iterated
   premise. *)
type site = { owner : string; span : Location.t; hit : Location.t }

(* A rule or function clause, with its written premises in order. *)
type body = { owner : string; at : Location.t; sites : site list }
type target = Rel of string | Rule of string * string | Prem of site
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

let string_of_line at =
  match at with
  | Location.Span { file; left; _ } -> Format.sprintf "%s:%d" file left.line
  | Location.Unknown | Location.File _ -> Location.to_string at

let string_of_target = function
  | Rel id -> id
  | Rule (id, rule_id) -> id ^ "/" ^ rule_id
  | Prem { owner; span; _ } -> string_of_line span ^ " in " ^ owner

(* What a [break] argument names, before it is checked against the spec. *)
type where = Def of target | Line of string option * int

let where_of_string name =
  let name = String.trim name in
  let def () =
    match String.split_on_char '/' name with
    | [ id ] when id <> "" -> Some (Def (Rel id))
    | [ id; rule_id ] when id <> "" && rule_id <> "" ->
        Some (Def (Rule (id, rule_id)))
    | _ -> None
  in
  match int_of_string_opt name with
  | Some line when line > 0 -> Some (Line (None, line))
  | Some _ -> None
  | None -> (
      match String.rindex_opt name ':' with
      | None -> def ()
      | Some idx -> (
          let file = String.sub name 0 idx in
          let line = String.sub name (idx + 1) (String.length name - idx - 1) in
          match int_of_string_opt line with
          | Some line when file <> "" && line > 0 ->
              Some (Line (Some file, line))
          | _ -> None))

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

(* An instruction the spec's author wrote as a premise, not one the algo pass
   synthesized. *)
let is_written (instr : Al.instr) =
  match instr.note with
  | Al.Premise _ -> true
  | Al.Sidecondition _ | Al.Input_sidecondition -> false

(* Premise index *)

(* An iterated premise is entered at the premise it iterates. *)
let rec entered_at (instr : Al.instr) =
  match instr.it with Al.IterI (instr, _) -> entered_at instr | _ -> instr.at

let body owner at instrs =
  let sites =
    List.filter is_written instrs
    |> List.map (fun (instr : Al.instr) ->
           { owner; span = instr.at; hit = entered_at instr })
  in
  { owner; at; sites }

let bodies_of_spec (spec : Al.spec) =
  List.concat_map
    (fun (def : Al.def) ->
      match def.it with
      | Al.RelD { relid; rules; _ } ->
          List.map
            (fun (rule : Al.rule) ->
              let owner =
                if rule.it.ruleid.it = "" then relid.it
                else relid.it ^ "/" ^ rule.it.ruleid.it
              in
              body owner rule.at rule.it.instrs)
            rules
      | Al.DecD { defid; clauses; _ } ->
          List.map
            (fun (clause : Al.clause) ->
              body ("$" ^ defid.it) clause.at clause.it.instrs)
            clauses
      | _ -> [])
    spec

let lines_of = function
  | Location.Span { left; right; _ } -> Some (left.line, right.line)
  | Location.Unknown | Location.File _ -> None

let covers line at =
  match lines_of at with
  | Some (first, last) -> first <= line && line <= last
  | None -> false

let starts_after line at =
  match lines_of at with Some (first, _) -> first > line | None -> false

(* The file of the spec that [name] names: the same path, or one ending in
   it. *)
let resolve_file bodies name =
  let files =
    List.filter_map (fun (body : body) -> Location.file body.at) bodies
    |> List.sort_uniq String.compare
  in
  let suffix = "/" ^ name in
  match
    List.filter (fun file -> file = name || String.ends_with ~suffix file) files
  with
  | [ file ] -> file
  | [] -> raise (Bad_command ("No rule or clause in `" ^ name ^ "`."))
  | files ->
      raise
        (Bad_command
           (Format.sprintf "`%s` matches several files: %s." name
              (String.concat ", " files)))

(* The written premise at [line] of [file], or else the next one in the same
   rule or clause. *)
let resolve_line bodies file line =
  let bodies =
    List.filter
      (fun (body : body) ->
        Location.file body.at = Some file && covers line body.at)
      bodies
  in
  let at_line =
    List.concat_map (fun (body : body) -> body.sites) bodies
    |> List.filter (fun site -> covers line site.span)
  in
  let latest a b =
    match (lines_of a.span, lines_of b.span) with
    | Some (first_a, _), Some (first_b, _) when first_b > first_a -> b
    | _ -> a
  in
  match (at_line, bodies) with
  | site :: sites, _ ->
      (* select the last premise covering the line *)
      List.fold_left latest site sites
  | [], [] ->
      raise
        (Bad_command (Format.sprintf "No rule or clause at %s:%d." file line))
  | [], body :: _ -> (
      (* no premise on the line, but it is inside a rule or clause *)
      match
        List.find_opt (fun site -> starts_after line site.span) body.sites
      with
      | Some site -> site
      | None ->
          raise
            (Bad_command
               (Format.sprintf "No premise at or after line %d in %s." line
                  body.owner)))

module State = struct
  let spec : Al.spec ref = ref []
  let bodies : body list ref = ref []
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
    let defined =
      match target with
      | Prem _ -> true
      | Rel _ | Rule _ -> defines !spec target
    in
    if not defined then
      raise
        (Bad_command
           (Format.sprintf "No definition matches `%s`."
              (string_of_target target)));
    match List.find_opt (fun b -> b.target = target) !breakpoints with
    | Some breakpoint -> `Existing breakpoint
    | None ->
        let breakpoint = { number = !next_number; target } in
        incr next_number;
        breakpoints := !breakpoints @ [ breakpoint ];
        `New breakpoint

  let delete_breakpoint number =
    if not (List.exists (fun b -> b.number = number) !breakpoints) then
      raise (Bad_command (Format.sprintf "No breakpoint number %d." number));
    breakpoints := List.filter (fun b -> b.number <> number) !breakpoints

  let breakpoint_for target =
    List.find_opt (fun b -> b.target = target) !breakpoints

  (* The breakpoint on the written premise entered at [at]. *)
  let breakpoint_at at =
    List.find_opt
      (fun b -> match b.target with Prem site -> site.hit = at | _ -> false)
      !breakpoints
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
  b, break WHERE     stop at WHERE: Rel, Rel/rule, or the premise at
                     FILE:LINE (LINE alone: in the current file)
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

(* The file a bare line number refers to: that of the current call. *)
let current_file () =
  match !State.frames with
  | frame :: _ -> (
      match Location.file (location_of_frame frame) with
      | Some file -> file
      | None -> raise (Bad_command "The current call has no source file."))
  | [] -> raise (Bad_command "No call in progress. Use FILE:LINE.")

let resolve_where name =
  match where_of_string name with
  | None ->
      raise
        (Bad_command
           ("Cannot read `" ^ name ^ "` as Rel, Rel/rule, FILE:LINE or LINE."))
  | Some (Def target) -> target
  | Some (Line (file, line)) ->
      let file =
        match file with
        | Some name -> resolve_file !State.bodies name
        | None -> current_file ()
      in
      Prem (resolve_line !State.bodies file line)

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
      | [ ("b" | "break"); name ] ->
          (match State.add_breakpoint (resolve_where name) with
          | `New { number; target } ->
              say "Breakpoint %d at %s." number (string_of_target target)
          | `Existing { number; target } ->
              say "Breakpoint %d is already at %s." number
                (string_of_target target));
          false
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

let say_source_line at =
  match at with
  | Location.Span { file; left; _ } -> (
      match lines_of_file file with
      | Some lines when left.line <= Array.length lines ->
          say "%7d  %s" left.line lines.(left.line - 1)
      | _ -> ())
  | Location.Unknown | Location.File _ -> ()

(* Stops once for the breakpoints [hits], all firing at the same event, which
   is always inside a call: one breakpoint, or a rule's and one on its first
   written premise. A premise among them also shows its line. *)
let stop_at hits =
  let name =
    match !State.frames with frame :: _ -> string_of_frame frame | [] -> ""
  in
  let numbers = List.map (fun { number; _ } -> string_of_int number) hits in
  say "Breakpoint%s %s, %s%s"
    (if List.length hits > 1 then "s" else "")
    (String.concat " and " numbers)
    name (current_call_site ());
  List.find_map
    (function { target = Prem site; _ } -> Some site.span | _ -> None)
    hits
  |> Option.iter say_source_line;
  pause ()

(* Stepping *)

(* Prints the first line of [at], as gdb does when stepping. *)
let say_line frame at =
  match at with
  | Location.Span { file; left; _ } ->
      say "%s, %s:%d" (string_of_frame frame) file left.line;
      say_source_line at
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
  Option.iter (fun hit -> stop_at [ hit ]) (State.breakpoint_for target)

(* Rule breakpoints. A rule is tried before it is known to apply: its
   conclusion's input patterns are checked by instructions that the algo pass
   puts first, with origin [Input_sidecondition]. Every instruction after them
   starts with a written premise, so a rule breakpoint is armed when the rule is
   entered and fires at its first written premise, or when the rule succeeds if
   it has none. *)

let arm_rule id rule_id =
  match !State.frames with
  | frame :: _ -> frame.armed <- State.breakpoint_for (Rule (id, rule_id))
  | [] -> ()

(* Disarms and returns the armed breakpoint, if any. *)
let take_armed () =
  match !State.frames with
  | frame :: _ ->
      let armed = frame.armed in
      frame.armed <- None;
      Option.to_list armed
  | [] -> []

(* A rule failing a guard never reached a written premise, so does not stop. *)
let exit_rule success =
  let hits = take_armed () in
  if success && hits <> [] then stop_at hits

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

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []
  let handle_value = None

  let init ~(spec : Instrumentation_api.Handler.spec) =
    State.reset ();
    (match spec with
    | AlSpec al_spec ->
        State.spec := al_spec;
        State.bodies := bodies_of_spec al_spec
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
          (* At most two breakpoints can be on the same premise. *)
          match take_armed () @ Option.to_list (State.breakpoint_at at) with
          | [] -> step_at at
          | hits -> stop_at hits)
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
