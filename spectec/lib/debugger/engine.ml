module Il = Lang.Il
module Location = Common.Location
module VEnv = Envs_interp.VEnv
open Common.Annotated

exception Error of string

let error fmt = Format.kasprintf (fun message -> raise (Error message)) fmt

type site = Sites.site = { owner : string; span : Location.t; hit : Location.t }

type target = Breakpoints.target =
  | Rel of string
  | Rule of string * string
  | Prem of site

type breakpoint = Breakpoints.breakpoint = { number : int; target : target }

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

module State = struct
  let spec : Al.spec ref = ref []
  let sites = ref Sites.empty
  let breakpoints = ref (Breakpoints.create ())
  let calls : call list ref = ref []
  let step : step option ref = ref None

  let reset al_spec =
    spec := al_spec;
    sites := Sites.index al_spec;
    breakpoints := Breakpoints.create ();
    calls := [];
    step := None
end

(* Breakpoints *)

let string_of_target = Breakpoints.string_of_target
let canonical = Sites.canonical

let or_error = function
  | Ok value -> value
  | Error message -> raise (Error message)

let spec_file file = Sites.file !State.sites file |> or_error

let premise_at ~file ~line =
  Sites.premise_at !State.sites ~file:(spec_file file) ~line |> or_error

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

let add_breakpoint target =
  let defined =
    match target with
    | Prem _ -> true
    | Rel _ | Rule _ -> defines !State.spec target
  in
  if not defined then
    error "No definition matches `%s`." (string_of_target target);
  Breakpoints.add !State.breakpoints target

let delete_breakpoint number =
  if not (Breakpoints.delete !State.breakpoints number) then
    error "No breakpoint number %d." number

let breakpoints () = Breakpoints.to_list !State.breakpoints

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
      (Breakpoints.find !State.breakpoints target)

  (* Rule breakpoints. A rule is tried before it is known to apply: its
     conclusion's input patterns are checked by premises that elaboration puts
     first and marks [Synthesized]. Every premise after them starts with a
     written one, so a rule breakpoint is armed when the rule is entered and
     fires at its first written premise, or when the rule succeeds if it has
     none. *)

  let arm_rule id rule_id =
    match !State.calls with
    | call :: _ ->
        call.armed <- Breakpoints.find !State.breakpoints (Rule (id, rule_id))
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
        if Sites.is_written prem then (
          update (fun call -> call.prem_at <- Some at);
          (* At most two breakpoints can be on the same premise. *)
          match
            take_armed ()
            @ Option.to_list (Breakpoints.on_premise !State.breakpoints at)
          with
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
