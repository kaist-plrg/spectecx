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
    Option.map (fun target -> Def target) (Breakpoints.target_of_string name)
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
      match Engine.find_local frame name with
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

(* The characters in the first [column] bytes of [text]. Columns count bytes,
   and a UTF-8 character starts at each byte that is not a continuation byte
   (10xxxxxx). *)
let chars_before text column =
  String.sub text 0 (min column (String.length text))
  |> String.fold_left
       (fun count char ->
         if Char.code char land 0xC0 = 0x80 then count else count + 1)
       0

(* Prints the first line of [at]. With [~failed], underlines [at] on it and
   says that it failed; an [at] that goes on past the line is underlined to
   the line's end, then [...]. *)
let say_source_line ?(failed = false) at =
  match at with
  | Location.Span { file; left; right } -> (
      match lines_of_file file with
      | Some lines when left.line <= Array.length lines ->
          let text = lines.(left.line - 1) in
          say "%7d  %s" left.line text;
          if failed then
            let start = chars_before text left.column in
            let stop, rest =
              if right.line = left.line then (chars_before text right.column, "")
              else (chars_before text (String.length text), "...")
            in
            say "         %s%s%s failed" (String.make start ' ')
              (String.make (max 1 (stop - start)) '^')
              rest
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

type action =
  | Plain of (running:bool -> Engine.resume option)
  | With_arg of string * (running:bool -> string -> Engine.resume option)

type command = { names : string list; summary : string; action : action }

let plain names summary run = { names; summary; action = Plain run }

let with_arg names arg summary run =
  { names; summary; action = With_arg (arg, run) }

let require_running ~running =
  if not running then raise (Bad_command "The run hasn't started.")

let start ~running =
  if running then raise (Bad_command "The run is already in progress.");
  Some Engine.Continue

let resume (resume : Engine.resume) ~running =
  require_running ~running;
  (match resume with
  | Continue -> ()
  | Step | Next | Finish ->
      if Engine.frames () = [] then raise (Bad_command "No call in progress."));
  Some resume

let inspect print ~running =
  require_running ~running;
  print ();
  None

let add_breakpoint ~running:_ name =
  (match Engine.add_breakpoint (resolve_where name) with
  | `New { number; target } ->
      say "Breakpoint %d at %s." number (Engine.string_of_target target)
  | `Existing { number; target } ->
      say "Breakpoint %d is already at %s." number
        (Engine.string_of_target target));
  None

let delete_breakpoint ~running:_ number =
  match int_of_string_opt number with
  | None -> raise (Bad_command ("Not a breakpoint number: " ^ number))
  | Some number ->
      Engine.delete_breakpoint number;
      None

let info ~running:_ = function
  | "b" | "breakpoints" ->
      print_breakpoints ();
      None
  | _ -> raise (Bad_command "Usage: info b")

let quit () = raise Engine.Quit

(* The command's names and its argument, such as [p, print NAME]. *)
let usage { names; action; _ } =
  let names = String.concat ", " names in
  match action with Plain _ -> names | With_arg (arg, _) -> names ^ " " ^ arg

let help_of commands =
  let indent = String.make 21 ' ' in
  "Commands:"
  :: List.map
       (fun command ->
         Format.sprintf "  %-19s%s" (usage command)
           (String.split_on_char '\n' command.summary
           |> String.concat ("\n" ^ indent)))
       commands
  |> String.concat "\n"

(* Lazy, so that [help] can list the commands it is one of. *)
let rec commands =
  lazy
    [
      plain [ "r"; "run" ] "start the run" start;
      plain [ "c"; "continue" ] "resume until the next breakpoint"
        (resume Continue);
      plain [ "s"; "step" ] "run to the next premise, entering calls"
        (resume Step);
      plain [ "n"; "next" ] "run to the next premise, stepping over calls"
        (resume Next);
      plain [ "f"; "finish" ] "run until the current call returns"
        (resume Finish);
      plain [ "bt"; "backtrace" ] "show the call stack, innermost call first"
        (inspect print_backtrace);
      plain [ "args" ] "show the current call's input values"
        (inspect print_args);
      plain [ "locals" ] "show the bindings in scope" (inspect print_locals);
      with_arg [ "p"; "print" ] "NAME" "show the binding of NAME"
        (fun ~running name -> inspect (fun () -> print_local name) ~running);
      plain [ "show" ] "show the source of the current rule or clause"
        (inspect print_current_source);
      with_arg [ "b"; "break" ] "WHERE"
        "stop at WHERE: Rel, Rel/rule, or the premise at\n\
         FILE:LINE (LINE alone: in the current file)"
        add_breakpoint;
      with_arg [ "d"; "delete" ] "N" "remove breakpoint N" delete_breakpoint;
      with_arg [ "info" ] "b" "list breakpoints" info;
      plain [ "q"; "quit" ] "abort the run" (fun ~running:_ -> quit ());
      plain [ "h"; "help" ] "show this help" (fun ~running:_ ->
          say "%s" (help_of (Lazy.force commands));
          None);
    ]

(* The command a line names, and the rest of the line as its argument. *)
let read_command () =
  Format.fprintf stdout "(dbg) %!";
  match In_channel.input_line stdin with
  | None ->
      say "";
      quit ()
  | Some line -> (
      if echo_commands then say "%s" line;
      let line = String.trim line in
      match String.index_opt line ' ' with
      | None -> (line, "")
      | Some idx ->
          ( String.sub line 0 idx,
            String.trim (String.sub line idx (String.length line - idx)) ))

let run_command ~running (name, arg) =
  match
    List.find_opt
      (fun { names; _ } -> List.mem name names)
      (Lazy.force commands)
  with
  | None -> raise (Bad_command ("Unknown command `" ^ name ^ "`. Try `help`."))
  | Some command -> (
      match (command.action, arg) with
      | Plain run, "" -> run ~running
      | Plain _, _ -> raise (Bad_command ("`" ^ name ^ "` takes no arguments."))
      | With_arg (_, run), arg when arg <> "" -> run ~running arg
      | With_arg (placeholder, _), _ ->
          raise (Bad_command ("Usage: " ^ name ^ " " ^ placeholder)))

(* Reads commands until one starts or resumes the run. Before the run,
   [running] is false and [run] starts it. *)
let rec prompt ~running : Engine.resume =
  let resumed =
    match read_command () with
    | "", _ -> None
    | line -> (
        try run_command ~running line
        with Bad_command message | Engine.Error message ->
          say "%s" message;
          None)
  in
  match resumed with Some resume -> resume | None -> prompt ~running

(* Stops *)

(* Prints where [name] is, then the first line of [at], as gdb does when
   stepping. [where] joins [name] and [at]'s file and line. *)
let say_line ?(where = ", ") ?failed name at =
  match at with
  | Location.Span { file; left; _ } ->
      say "%s%s%s:%d" name where file left.line;
      say_source_line ?failed at
  | Location.Unknown | Location.File _ -> say "%s" name

let current_name () =
  match Engine.frames () with frame :: _ -> frame.name | [] -> ""

let describe_stop : Engine.stop_reason -> unit = function
  | Breakpoints hits -> (
      let numbers =
        List.map
          (fun ({ number; _ } : Engine.breakpoint) -> string_of_int number)
          hits
      in
      say "Breakpoint%s %s, %s%s"
        (if List.length hits > 1 then "s" else "")
        (String.concat " and " numbers)
        (current_name ()) (current_call_site ());
      match Engine.frames () with
      | { premise = Some at; _ } :: _ -> say_source_line at
      | _ -> ())
  | Stepped at -> say_line (current_name ()) at
  | Premise_failed at ->
      say_line ~where:" failed at " ~failed:true (current_name ()) at
  | Returned { name; result = Failed } -> say "%s failed." name
  | Returned { name; result = Rel_outputs [] } -> say "%s holds." name
  | Returned { name; result = Rel_outputs outputs } ->
      say "%s returned:" name;
      say_values outputs
  | Returned { name; result = Func_output value } ->
      say_value (name ^ " returned ") value

let before_run () =
  say "Set breakpoints, then `run` to start. `help` lists commands.";
  ignore (prompt ~running:false)

let stopped stop =
  describe_stop stop;
  prompt ~running:true

let after_run () = ()

let config : Instrumentation_config.Handler_config.t =
  {
    name = "debugger";
    modes = [ `AL ];
    handler = Engine.handler ~before_run ~stopped ~after_run;
    output = Instrumentation_api.Output.stdout;
  }
