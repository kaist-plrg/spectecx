module Al = Lang.Al
module Il = Lang.Il
module Location = Common.Location
module VEnv = Envs_interp.VEnv
open Common.Annotated

exception Error of string
exception Quit

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
  | Premise_failed of Location.t
  | Returned of { name : string; result : call_result }

type resume = Continue | Step | Next | Finish

(* What a call runs: a relation, which tries rules, or a function, which tries
   clauses. *)
type callee =
  | Relation of {
      rule_id : string option;
      (* A breakpoint on the current rule that has not fired in this attempt. *)
      armed : breakpoint option;
    }
  | Function of { clause_idx : int option }

(* A call in progress. *)
type call = {
  callee : callee;
  id : string;
  at : Location.t;
  inputs : Il.Value.t list;
  (* The bindings in scope, as of the last event in this call. *)
  env : VEnv.t;
  (* The written premise this call is at. *)
  prem_at : Location.t option;
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

  (* Set once the frontend quits. The run then unwinds, and the interpreter
     emits the exits it skips, which must not stop the run again. *)
  let quit = ref false

  let reset al_spec =
    spec := al_spec;
    sites := Sites.index al_spec;
    breakpoints := Breakpoints.create ();
    calls := [];
    step := None;
    quit := false
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
  match call.callee with
  | Relation { rule_id = Some rule_id; _ } when rule_id <> "" ->
      call.id ^ "/" ^ rule_id
  | Relation _ -> call.id
  | Function _ -> "$" ^ call.id

(* The definition of the relation or function a call runs. *)
let find_def call =
  List.find_opt
    (fun (def : Al.def) ->
      match (call.callee, def.it) with
      | Relation _, Al.RelD { relid; _ } -> relid.it = call.id
      | Function _, (Al.DecD { defid; _ } | Al.BuiltinDecD { defid; _ }) ->
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
        match (def.it, call.callee) with
        | Al.RelD { rules; _ }, Relation { rule_id = Some rule_id; _ } ->
            List.find_opt
              (fun (rule : Al.rule) -> rule.it.ruleid.it = rule_id)
              rules
            |> Option.map (fun (rule : Al.rule) -> rule.at)
        | Al.DecD { clauses; _ }, Function { clause_idx = Some idx } ->
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

let find_local (frame : frame) name =
  let bindings = VEnv.bindings frame.env in
  match
    List.filter (fun (var, _) -> Envs_interp.Var.to_string var = name) bindings
  with
  | [] -> List.filter (fun (((id : Il.id), _), _) -> id.it = name) bindings
  | found -> found

(* Running *)

let depth () = List.length !State.calls

let handler ~before_run ~stopped ~after_run :
    (module Instrumentation_api.Handler.S) =
  (module struct
    let handle_value = None

    (* Every stop ends the step in progress; the frontend picks the next one. *)
    let stop reason =
      if not !State.quit then
        State.step :=
          match stopped reason with
          | Continue -> None
          | Step -> Some Step_in
          | Next -> Some (Step_over (depth ()))
          | Finish -> Some (Step_out (depth ()))
          | exception Quit ->
              State.quit := true;
              raise Quit

    (* Stops for [reason] when the step in progress stops at this depth. *)
    let step_at reason =
      match !State.step with
      | Some Step_in -> stop reason
      | Some (Step_over depth') when depth () <= depth' -> stop reason
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

    (* Replaces the innermost call with [f] of it. *)
    let update f =
      match !State.calls with
      | call :: calls -> State.calls := f call :: calls
      | [] -> ()

    (* Rule breakpoints. A rule is tried before it is known to apply: its
     conclusion's input patterns are checked by guards that the algo pass puts
     first, as synthesized instructions. So a rule breakpoint is armed when the
     rule is entered, and stops the run at the rule's first written premise, or
     when the rule succeeds if it reaches none. *)

    (* Disarms and returns the armed breakpoint, if any. *)
    let take_armed () =
      match !State.calls with
      | { callee = Relation r; _ } :: _ ->
          update (fun call ->
              { call with callee = Relation { r with armed = None } });
          Option.to_list r.armed
      | _ -> []

    (* If a breakpoint is still armed when its rule exits, the run stops only if
       the rule succeeded. *)
    let exit_rule success =
      let hits = take_armed () in
      if success && hits <> [] then stop (Breakpoints hits)

    let push_call callee id at inputs =
      State.calls :=
        { callee; id; at; inputs; env = VEnv.empty; prem_at = None }
        :: !State.calls

    (* A rule or clause ended: nothing from it stays in scope. *)
    let leave_scope () =
      update (fun call -> { call with env = VEnv.empty; prem_at = None })

    let rel_result success conclusion =
      if success then
        Rel_outputs (Il.Mode.outputs conclusion |> List.filter_map Fun.id)
      else Failed

    let func_result = function
      | Some value -> Func_output value
      | None -> Failed

    let static_dependencies = []

    let init ~(spec : Instrumentation_api.Handler.spec) =
      (match spec with
      | AlSpec al_spec -> State.reset al_spec
      | SlSpec _ -> invalid_arg "Debugger.init: the debug subcommand runs AL");
      before_run ()

    let finish () = after_run ()

    let handle : Instrumentation_api.Event.t -> unit = function
      | Rel_enter { id; at; inputs } ->
          push_call (Relation { rule_id = None; armed = None }) id at inputs;
          stop_if_breakpoint (Rel id)
      | Rel_exit { success; conclusion; _ } ->
          leave_call (rel_result success conclusion)
      | Func_exit { output; _ } -> leave_call (func_result output)
      | Rule_enter { id; rule_id; at = _; env } ->
          let armed =
            Breakpoints.find !State.breakpoints (Rule (id, rule_id))
          in
          update (fun call ->
              match call.callee with
              | Relation _ ->
                  {
                    call with
                    callee = Relation { rule_id = Some rule_id; armed };
                    env;
                  }
              | Function _ -> { call with env })
      | Rule_exit { success; _ } ->
          exit_rule success;
          update (fun call ->
              match call.callee with
              | Relation r ->
                  { call with callee = Relation { r with rule_id = None } }
              | Function _ -> call);
          leave_scope ()
      | Instr_enter { instr = AlInstr prem; at; env } -> (
          let written = not (Al.is_synthesized prem) in
          update (fun call ->
              if written then { call with env; prem_at = Some at }
              else { call with env });
          if written then
            (* At most two breakpoints can be on the same premise. *)
            match
              take_armed ()
              @ Option.to_list (Breakpoints.on_premise !State.breakpoints at)
            with
            | [] -> step_at (Stepped at)
            | hits -> stop (Breakpoints hits))
      | Instr_exit { instr = AlInstr instr; success; env; _ } -> (
          update (fun call -> { call with env });
          match instr.note with
          | (Al.Premise prem | Al.Sidecondition prem) when not success ->
              step_at (Premise_failed prem.at)
          | Al.Premise _ | Al.Sidecondition _ | Al.Input_sidecondition -> ())
      | Func_enter { id; at; inputs } ->
          push_call (Function { clause_idx = None }) id at inputs
      | Clause_enter { clause_idx; env; _ } ->
          update (fun call ->
              match call.callee with
              | Function _ ->
                  {
                    call with
                    callee = Function { clause_idx = Some clause_idx };
                    env;
                  }
              | Relation _ -> { call with env })
      | Clause_exit _ ->
          update (fun call ->
              match call.callee with
              | Function _ ->
                  { call with callee = Function { clause_idx = None } }
              | Relation _ -> call);
          leave_scope ()
      | Test_start _ | Test_end _ | Repetition_enter _ | Repetition_exit _
      | Case_match _ | Instr_enter _ | Instr_exit _ ->
          ()
  end)
