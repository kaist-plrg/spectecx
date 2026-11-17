module Il = Lang.Il
module Location = Common.Location
module VEnv = Envs_interp.VEnv

exception Bad_command of string

let stdout = Format.std_formatter
let say fmt = Format.fprintf stdout (fmt ^^ "\n%!")
let echo_commands = not (Unix.isatty Unix.stdin)

(* Parsing breakpoints *)

(* What a [break] argument names, before it is checked against the spec. *)
type where = Def of Engine.target | Line of string option * int

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

(* The file a bare line number refers to: that of the current call. *)
let current_file () =
  match Engine.frames () with
  | frame :: _ -> (
      match Location.file frame.location with
      | Some file -> file
      | None -> raise (Bad_command "The current call has no source file."))
  | [] -> raise (Bad_command "No call in progress. Use FILE:LINE.")

let resolve_where name : Engine.target =
  match where_of_string name with
  | None ->
      raise
        (Bad_command
           ("Cannot read `" ^ name ^ "` as Rel, Rel/rule, FILE:LINE or LINE."))
  | Some (Def target) -> target
  | Some (Line (file, line)) ->
      let file =
        match file with Some file -> file | None -> current_file ()
      in
      Prem (Engine.premise_at ~file ~line)

(* Printing calls *)

let describe_call_site at =
  match at with
  | Location.Unknown -> ""
  | at -> " (called at " ^ Location.to_string at ^ ")"

let current_call_site () =
  match Engine.frames () with
  | frame :: _ -> describe_call_site frame.call_site
  | [] -> ""

let print_backtrace () =
  List.iteri
    (fun depth (frame : Engine.frame) ->
      say "#%d  %s%s" depth frame.name (describe_call_site frame.call_site))
    (Engine.frames ())

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
  match Engine.frames () with
  | [] -> say "No call in progress."
  | frame :: _ when frame.inputs = [] -> say "%s takes no inputs." frame.name
  | frame :: _ -> say_values frame.inputs

let say_binding (var, value) =
  say_value ("  " ^ Envs_interp.Var.to_string var ^ " = ") value

let print_locals () =
  match Engine.frames () with
  | [] -> say "No call in progress."
  | frame :: _ when VEnv.is_empty frame.env -> say "No local bindings."
  | frame :: _ ->
      VEnv.iter (fun var value -> say_binding (var, value)) frame.env

(* Prints the bindings of [name], one per iteration depth. *)
let print_local name =
  match Engine.frames () with
  | [] -> say "No call in progress."
  | frame :: _ -> (
      match
        VEnv.bindings frame.env
        |> List.filter (fun (((id : Il.id), _), _) -> id.it = name)
      with
      | [] -> raise (Bad_command ("No local `" ^ name ^ "`."))
      | bindings -> List.iter say_binding bindings)

(* Source *)

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
  match Engine.frames () with
  | [] -> say "No call in progress."
  | frame :: _ -> print_source ?current:frame.premise frame.location

let say_source_line at =
  match at with
  | Location.Span { file; left; _ } -> (
      match lines_of_file file with
      | Some lines when left.line <= Array.length lines ->
          say "%7d  %s" left.line lines.(left.line - 1)
      | _ -> ())
  | Location.Unknown | Location.File _ -> ()

let print_breakpoints () =
  match Engine.breakpoints () with
  | [] -> say "No breakpoints."
  | breakpoints ->
      List.iter
        (fun ({ number; target } : Engine.breakpoint) ->
          say "  %d  %s" number (Engine.string_of_target target))
        breakpoints

(* Commands *)

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

(* Reads commands until one starts or resumes the run. Before the run,
   [running] is false and [run] starts it. *)
let rec prompt ~running : Engine.resume =
  let require_running () =
    if not running then raise (Bad_command "The run hasn't started.")
  in
  let resume (resume : Engine.resume) =
    require_running ();
    (match resume with
    | Continue -> ()
    | Step | Next | Finish ->
        if Engine.frames () = [] then raise (Bad_command "No call in progress."));
    Some resume
  in
  let inspect print =
    require_running ();
    print ();
    None
  in
  let resumed =
    try
      match read_command () with
      | [ ("r" | "run") ] ->
          if running then raise (Bad_command "The run is already in progress.");
          Some Engine.Continue
      | [ ("c" | "continue") ] -> resume Continue
      | [ ("s" | "step") ] -> resume Step
      | [ ("n" | "next") ] -> resume Next
      | [ ("f" | "finish") ] -> resume Finish
      | [ ("bt" | "backtrace") ] -> inspect print_backtrace
      | [ "args" ] -> inspect print_args
      | [ "show" ] -> inspect print_current_source
      | [ "locals" ] -> inspect print_locals
      | [ ("p" | "print"); name ] -> inspect (fun () -> print_local name)
      | [ ("b" | "break"); name ] ->
          (match Engine.add_breakpoint (resolve_where name) with
          | `New { number; target } ->
              say "Breakpoint %d at %s." number (Engine.string_of_target target)
          | `Existing { number; target } ->
              say "Breakpoint %d is already at %s." number
                (Engine.string_of_target target));
          None
      | [ ("d" | "delete"); number ] -> (
          match int_of_string_opt number with
          | None -> raise (Bad_command ("Not a breakpoint number: " ^ number))
          | Some number ->
              Engine.delete_breakpoint number;
              None)
      | [ "info"; ("b" | "breakpoints") ] ->
          print_breakpoints ();
          None
      | [ ("q" | "quit") ] -> quit ()
      | [ ("h" | "help") ] ->
          say "%s" help;
          None
      | [] -> None
      | command :: _ ->
          raise (Bad_command ("Unknown command `" ^ command ^ "`. Try `help`."))
    with Bad_command message | Engine.Error message ->
      say "%s" message;
      None
  in
  match resumed with Some resume -> resume | None -> prompt ~running

(* Stops *)

(* Prints the first line of [at], as gdb does when stepping. *)
let say_line name at =
  match at with
  | Location.Span { file; left; _ } ->
      say "%s, %s:%d" name file left.line;
      say_source_line at
  | Location.Unknown | Location.File _ -> say "%s" name

let current_name () =
  match Engine.frames () with frame :: _ -> frame.name | [] -> ""

let describe_stop : Engine.stop_reason -> unit = function
  | Breakpoints hits ->
      let numbers =
        List.map
          (fun ({ number; _ } : Engine.breakpoint) -> string_of_int number)
          hits
      in
      say "Breakpoint%s %s, %s%s"
        (if List.length hits > 1 then "s" else "")
        (String.concat " and " numbers)
        (current_name ()) (current_call_site ());
      List.find_map
        (function
          | ({ target = Prem site; _ } : Engine.breakpoint) -> Some site.span
          | _ -> None)
        hits
      |> Option.iter say_source_line
  | Stepped at -> say_line (current_name ()) at
  | Returned { name; result = Failed } -> say "%s failed." name
  | Returned { name; result = Rel_outputs [] } -> say "%s holds." name
  | Returned { name; result = Rel_outputs outputs } ->
      say "%s returned:" name;
      say_values outputs
  | Returned { name; result = Func_output value } ->
      say_value (name ^ " returned ") value

module Frontend = struct
  let before_run () =
    say "Set breakpoints, then `run` to start. `help` lists commands.";
    ignore (prompt ~running:false)

  let stopped stop =
    describe_stop stop;
    prompt ~running:true

  let after_run () = ()
end

module Handler = Engine.Make (Frontend)

let config : Instrumentation_config.Handler_config.t =
  {
    name = "debugger";
    modes = [ `IL ];
    handler = (module Handler : Instrumentation_api.Handler.S);
    output = Instrumentation_api.Output.stdout;
  }
