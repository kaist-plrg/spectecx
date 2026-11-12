module Il = Lang.Il
module Location = Common.Location
module VEnv = Envs_interp.VEnv
open Common.Annotated

exception Error of string

let error fmt = Format.kasprintf (fun message -> raise (Error message)) fmt

type site = { owner : string; span : Location.t; hit : Location.t }

(* A rule or function clause, with its written premises in order. *)
type body = { owner : string; at : Location.t; sites : site list }
type target = Rel of string | Rule of string * string | Prem of site
type breakpoint = { number : int; target : target }

type frame = {
  name : string;
  call_site : Location.t;
  location : Location.t;
  premise : Location.t option;
  inputs : Il.Value.t list;
  env : VEnv.t;
}

type call_result =
  | Failed
  | Rel_outputs of Il.Value.t list
  | Func_output of Il.Value.t

type stop_reason =
  | Breakpoints of breakpoint list
  | Stepped of Location.t
  | Returned of { name : string; result : call_result }

type resume = Continue | Step | Next | Finish

module type FRONTEND = sig
  val before_run : unit -> unit
  val stopped : stop_reason -> resume
  val after_run : unit -> unit
end

(* A call in progress. *)
type call = {
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

(* Where a step stops, by call depth: [Step_in] at the next premise anywhere,
   [Step_over] at the next one in a call no deeper, and [Step_out] when the
   call at that depth returns. *)
type step = Step_in | Step_over of int | Step_out of int

let string_of_line at =
  match at with
  | Location.Span { file; left; _ } -> Format.sprintf "%s:%d" file left.line
  | Location.Unknown | Location.File _ -> Location.to_string at

let string_of_target = function
  | Rel id -> id
  | Rule (id, rule_id) -> id ^ "/" ^ rule_id
  | Prem { owner; span; _ } -> string_of_line span ^ " in " ^ owner

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
let rec entered_at (prem : Al.instr) =
  match prem.it with Al.IterI (prem, _) -> entered_at prem | _ -> prem.at

let body owner at prems =
  let sites =
    List.filter is_written prems
    |> List.map (fun (prem : Al.instr) ->
           { owner; span = prem.at; hit = entered_at prem })
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
  | [] -> error "No rule or clause in `%s`." name
  | files ->
      error "`%s` matches several files: %s." name (String.concat ", " files)

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
  | [], [] -> error "No rule or clause at %s:%d." file line
  | [], body :: _ -> (
      (* no premise on the line, but it is inside a rule or clause *)
      match
        List.find_opt (fun site -> starts_after line site.span) body.sites
      with
      | Some site -> site
      | None -> error "No premise at or after line %d in %s." line body.owner)

module State = struct
  let spec : Al.spec ref = ref []
  let bodies : body list ref = ref []
  let breakpoints : breakpoint list ref = ref []
  let next_number = ref 1
  let calls : call list ref = ref []
  let step : step option ref = ref None

  let reset al_spec =
    spec := al_spec;
    bodies := bodies_of_spec al_spec;
    breakpoints := [];
    next_number := 1;
    calls := [];
    step := None

  let breakpoint_for target =
    List.find_opt (fun b -> b.target = target) !breakpoints

  (* The breakpoint on the written premise entered at [at]. *)
  let breakpoint_at at =
    List.find_opt
      (fun b -> match b.target with Prem site -> site.hit = at | _ -> false)
      !breakpoints
end

(* Breakpoints *)

let premise_at ~file ~line =
  let file = resolve_file !State.bodies file in
  resolve_line !State.bodies file line

let add_breakpoint target =
  let defined =
    match target with
    | Prem _ -> true
    | Rel _ | Rule _ -> defines !State.spec target
  in
  if not defined then
    error "No definition matches `%s`." (string_of_target target);
  match State.breakpoint_for target with
  | Some breakpoint -> `Existing breakpoint
  | None ->
      let breakpoint = { number = !State.next_number; target } in
      incr State.next_number;
      State.breakpoints := !State.breakpoints @ [ breakpoint ];
      `New breakpoint

let delete_breakpoint number =
  if not (List.exists (fun b -> b.number = number) !State.breakpoints) then
    error "No breakpoint number %d." number;
  State.breakpoints :=
    List.filter (fun b -> b.number <> number) !State.breakpoints

let breakpoints () = !State.breakpoints

(* Frames *)

let name_of_call call =
  match (call.kind, call.rule_id) with
  | `Rel, Some rule_id when rule_id <> "" -> call.id ^ "/" ^ rule_id
  | `Rel, _ -> call.id
  | `Func, _ -> "$" ^ call.id

(* The definition of the relation or function a call runs. *)
let find_def call =
  List.find_opt
    (fun (def : Al.def) ->
      match (call.kind, def.it) with
      | `Rel, Al.RelD { relid; _ } -> relid.it = call.id
      | `Func, (Al.DecD { defid; _ } | Al.BuiltinDecD { defid; _ }) ->
          defid.it = call.id
      | _ -> false)
    !State.spec

(* Where a call is in the spec: its current rule or clause, or else its
   definition. *)
let location_of_call call =
  match find_def call with
  | None -> Location.Unknown
  | Some def -> (
      let inner =
        match (def.it, call.rule_id, call.clause_idx) with
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

let frame_of_call call =
  {
    name = name_of_call call;
    call_site = call.at;
    location = location_of_call call;
    premise = call.prem_at;
    inputs = call.inputs;
    env = call.env;
  }

let frames () = List.map frame_of_call !State.calls

(* Running *)

let depth () = List.length !State.calls

module Make (F : FRONTEND) : Instrumentation_api.Handler.S = struct
  (* Every stop ends the step in progress; the frontend picks the next one. *)
  let stop reason =
    State.step :=
      match F.stopped reason with
      | Continue -> None
      | Step -> Some Step_in
      | Next -> Some (Step_over (depth ()))
      | Finish -> Some (Step_out (depth ()))

  (* Stops at a written premise when the step in progress lands on it. *)
  let step_at at =
    match !State.step with
    | Some Step_in -> stop (Stepped at)
    | Some (Step_over depth') when depth () <= depth' -> stop (Stepped at)
    | _ -> ()

  (* Pops the innermost call, stopping when it is the one being finished. *)
  let leave_call result =
    let finishing =
      match !State.step with
      | Some (Step_out depth') -> depth () = depth'
      | _ -> false
    in
    match !State.calls with
    | call :: calls ->
        State.calls := calls;
        if finishing then stop (Returned { name = name_of_call call; result })
    | [] -> ()

  let stop_if_breakpoint target =
    Option.iter
      (fun hit -> stop (Breakpoints [ hit ]))
      (State.breakpoint_for target)

  (* Rule breakpoints. A rule is tried before it is known to apply: its
     conclusion's input patterns are checked by premises that elaboration puts
     first and marks [Synthesized]. Every premise after them starts with a
     written one, so a rule breakpoint is armed when the rule is entered and
     fires at its first written premise, or when the rule succeeds if it has
     none. *)

  let arm_rule id rule_id =
    match !State.calls with
    | call :: _ -> call.armed <- State.breakpoint_for (Rule (id, rule_id))
    | [] -> ()

  (* Disarms and returns the armed breakpoint, if any. *)
  let take_armed () =
    match !State.calls with
    | call :: _ ->
        let armed = call.armed in
        call.armed <- None;
        Option.to_list armed
    | [] -> []

  (* A rule failing a guard never reached a written premise, so does not
     stop. *)
  let exit_rule success =
    let hits = take_armed () in
    if success && hits <> [] then stop (Breakpoints hits)

  let push_call kind id at inputs =
    State.calls :=
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
      :: !State.calls

  let update f = match !State.calls with call :: _ -> f call | [] -> ()

  (* A rule or clause ended: nothing from it stays in scope. *)
  let leave_scope () =
    update (fun call ->
        call.env <- VEnv.empty;
        call.prem_at <- None)

  let rel_result success conclusion =
    if success then
      Rel_outputs (Il.Mode.outputs conclusion |> List.filter_map Fun.id)
    else Failed

  let func_result = function Some value -> Func_output value | None -> Failed
  let static_dependencies = []
  let handle_value = None

  let init ~(spec : Instrumentation_api.Handler.spec) =
    (match spec with
    | AlSpec al_spec -> State.reset al_spec
    | SlSpec _ -> invalid_arg "Debugger.init: the debug subcommand runs AL");
    F.before_run ()

  let finish () = F.after_run ()

  let handle : Instrumentation_api.Event.t -> unit = function
    | Rel_enter { id; at; inputs } ->
        push_call `Rel id at inputs;
        stop_if_breakpoint (Rel id)
    | Rel_exit { success; conclusion; _ } ->
        leave_call (rel_result success conclusion)
    | Func_exit { output; _ } -> leave_call (func_result output)
    | Rule_enter { id; rule_id; at = _; env } ->
        update (fun call ->
            call.rule_id <- Some rule_id;
            call.env <- env);
        arm_rule id rule_id
    | Rule_exit { success; _ } ->
        exit_rule success;
        update (fun call -> call.rule_id <- None);
        leave_scope ()
    | Instr_enter { instr = AlInstr prem; at; env } ->
        update (fun call -> call.env <- env);
        if is_written prem then (
          update (fun call -> call.prem_at <- Some at);
          (* At most two breakpoints can be on the same premise. *)
          match take_armed () @ Option.to_list (State.breakpoint_at at) with
          | [] -> step_at at
          | hits -> stop (Breakpoints hits))
    | Instr_exit { instr = AlInstr _; env; _ } ->
        update (fun call -> call.env <- env)
    | Func_enter { id; at; inputs } -> push_call `Func id at inputs
    | Clause_enter { clause_idx; env; _ } ->
        update (fun call ->
            call.clause_idx <- Some clause_idx;
            call.env <- env)
    | Clause_exit _ ->
        update (fun call -> call.clause_idx <- None);
        leave_scope ()
    | Test_start _ | Test_end _ | Repetition_enter _ | Repetition_exit _
    | Case_match _ | Instr_enter _ | Instr_exit _ ->
        ()
end
