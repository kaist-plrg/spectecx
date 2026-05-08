(** Tree: buffered derivation-tree renderer for a successful evaluation.

    Unlike {!Trace} (which streams every event), Tree accumulates events inside
    a top-level relation invocation and emits a single ASCII tree once that
    invocation completes. Failed rule attempts are pruned: only the rule that
    actually fired at each relation invocation remains visible.

    Levels:
    - [Rules]: tree of relation/function calls, each relation node tagged with
      the rule that matched.
    - [Inputs]: above + input values on each node and the output on success, one
      value per line just under the node label. *)

module Il = Lang.Il
open Util

type level = Rules | Inputs
type config = { level : level; output : Instrumentation_api.Output.t }

let default_config =
  { level = Rules; output = Instrumentation_api.Output.stdout }

let config = ref default_config
let fmt = ref Format.std_formatter

let summarize_value (v : Il.Value.t) : string =
  Il.Print.string_of_value v |> summarize ~max_len:100

(* === Spec lookup for notation rendering ============================ *)

(* Per-relation notation skeleton + which arg slots are inputs.
   Populated from the IL spec at init; empty under SL. *)
module Sig = struct
  let relations : (string, Il.Mixfix.mixop * int list) Hashtbl.t =
    Hashtbl.create 32

  let functions : (string, int) Hashtbl.t = Hashtbl.create 32

  let reset () =
    Hashtbl.clear relations;
    Hashtbl.clear functions

  let collect_il (spec : Il.spec) : unit =
    List.iter
      (fun (def : Il.def) ->
        match def.it with
        | RelD (id, nottyp, input_idxs, _rules) ->
            let mixop = Il.Mixfix.to_mixop nottyp.it in
            Hashtbl.replace relations id.it (mixop, input_idxs)
        | DecD (id, _tparams, params, _ret, _) ->
            Hashtbl.replace functions id.it (List.length params)
        | _ -> ())
      spec

  let lookup_relation id = Hashtbl.find_opt relations id
  let knows_function id = Hashtbl.mem functions id
end

(* === Tree representation =========================================== *)

type kind = Rel | Func
type outcome = Failed | Succeeded of Il.Value.t list

type node = {
  kind : kind;
  id : string;
  inputs : Il.Value.t list;
  mutable rule : string option;
  mutable outcome : outcome;
  mutable children_rev : node list;
  mutable rollback_children : node list option;
}

let new_node kind id inputs =
  {
    kind;
    id;
    inputs;
    rule = None;
    outcome = Failed;
    children_rev = [];
    rollback_children = None;
  }

let outcome_of_outputs = function
  | Some outputs -> Succeeded outputs
  | None -> Failed

let outcome_of_output = function Some v -> Succeeded [ v ] | None -> Failed

(* === Mutable state ================================================= *)

module State = struct
  let stack : node list ref = ref []
  let reset () = stack := []
  let push node = stack := node :: !stack

  let close_top ~outcome =
    match !stack with
    | [] -> assert false
    | top :: rest -> (
        top.outcome <- outcome;
        stack := rest;
        match rest with
        | [] -> Some top
        | parent :: _ ->
            parent.children_rev <- top :: parent.children_rev;
            None)

  let begin_rule_attempt () =
    match !stack with
    | [] -> ()
    | top :: _ -> top.rollback_children <- Some top.children_rev

  let end_rule_attempt ~rule_id ~success =
    match !stack with
    | [] -> ()
    | top :: _ ->
        if success then top.rule <- Some rule_id
        else
          Option.iter
            (fun saved -> top.children_rev <- saved)
            top.rollback_children;
        top.rollback_children <- None
end

(* === Rendering ===================================================== *)

(* Weave inputs (in [input_idxs] order) and outputs (filling the
   complement positions, in increasing order) into a single
   arg-position list of length [arity]. Returns [None] if lengths don't
   line up so the caller can fall back. *)
let weave_args ~arity ~input_idxs ~inputs ~outputs : Il.Value.t list option =
  let output_idxs =
    List.init arity Fun.id |> List.filter (fun i -> not (List.mem i input_idxs))
  in
  if
    List.length input_idxs <> List.length inputs
    || List.length output_idxs <> List.length outputs
  then None
  else
    let by_pos =
      List.combine input_idxs inputs @ List.combine output_idxs outputs
    in
    Some (List.init arity (fun i -> List.assoc i by_pos))

let render_notation mixop args =
  let mixfix = Il.Mixfix.fill mixop args in
  Il.Mixfix.render
    ~string_of_atom:(fun a -> Xl.Atom.string_of_atom a.it)
    ~string_of_arg:summarize_value mixfix

(* Try to render a relation node as its declared notation with values
   substituted; fall back to [id/rule] if the notation isn't available
   (e.g., under SL) or the outcome is Failed. *)
let try_render_relation_with_notation node =
  match (Sig.lookup_relation node.id, node.outcome) with
  | Some (mixop, input_idxs), Succeeded outputs -> (
      let arity = Il.Mixfix.arity mixop in
      match weave_args ~arity ~input_idxs ~inputs:node.inputs ~outputs with
      | Some args -> Some (render_notation mixop args)
      | None -> None)
  | _ -> None

let label node =
  let base =
    match node.kind with
    | Rel -> (
        match try_render_relation_with_notation node with
        | Some notation -> (
            match node.rule with
            | Some r when r <> "" ->
                Format.sprintf "%s    [%s/%s]" notation node.id r
            | _ -> Format.sprintf "%s    [%s]" notation node.id)
        | None -> (
            match node.rule with
            | Some r when r <> "" -> node.id ^ "/" ^ r
            | _ -> node.id))
    | Func ->
        let args = List.map summarize_value node.inputs |> String.concat ", " in
        let call = Format.sprintf "$%s(%s)" node.id args in
        if Sig.knows_function node.id then
          match node.outcome with
          | Succeeded [ v ] -> Format.sprintf "%s = %s" call (summarize_value v)
          | _ -> call
        else "$" ^ node.id
  in
  match node.outcome with Succeeded _ -> base | Failed -> base ^ "  ✗"

(* The leader [│   ] (vs spaces) keeps the vertical line down to the first
   child unbroken when there are children below. *)
let render_decorations ~prefix ~has_children node out =
  if !config.level = Inputs then (
    let leader = if has_children then "│   " else "    " in
    let line tag v =
      Format.fprintf out "%s%s%s: %s\n" prefix leader tag (summarize_value v)
    in
    List.iter (line "in") node.inputs;
    match node.outcome with
    | Succeeded outputs -> List.iter (line "out") outputs
    | Failed -> ())

let rec render_child ~prefix ~is_last node out =
  let connector = if is_last then "└── " else "├── " in
  Format.fprintf out "%s%s%s\n" prefix connector (label node);
  let child_prefix = prefix ^ if is_last then "    " else "│   " in
  let has_children = node.children_rev <> [] in
  render_decorations ~prefix:child_prefix ~has_children node out;
  render_children ~prefix:child_prefix node out

and render_children ~prefix node out =
  let children = List.rev node.children_rev in
  let n = List.length children in
  List.iteri
    (fun i c -> render_child ~prefix ~is_last:(i = n - 1) c out)
    children

let render_root node =
  let out = !fmt in
  Format.fprintf out "%s\n" (label node);
  render_decorations ~prefix:"" ~has_children:(node.children_rev <> []) node out;
  render_children ~prefix:"" node out;
  Format.pp_print_flush out ()

let close_and_maybe_render ~outcome =
  Option.iter render_root (State.close_top ~outcome)

(* === Handler module ================================================ *)

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []

  let init ~spec =
    State.reset ();
    Sig.reset ();
    match spec with
    | Instrumentation_api.Handler.IlSpec s -> Sig.collect_il s
    | Instrumentation_api.Handler.SlSpec _ -> ()

  let finish () = ()

  let handle : Instrumentation_api.Event.t -> unit = function
    | Test_start _ | Test_end _ -> State.reset ()
    | Rel_enter { id; at = _; inputs } -> State.push (new_node Rel id inputs)
    | Rel_exit { id = _; at = _; outputs } ->
        close_and_maybe_render ~outcome:(outcome_of_outputs outputs)
    | Rule_enter _ -> State.begin_rule_attempt ()
    | Rule_exit { id = _; rule_id; at = _; success } ->
        State.end_rule_attempt ~rule_id ~success
    | Func_enter { id; at = _; inputs } -> State.push (new_node Func id inputs)
    | Func_exit { id = _; at = _; output } ->
        close_and_maybe_render ~outcome:(outcome_of_output output)
    | Clause_enter _ | Clause_exit _ -> ()
    | Iter_prem_enter _ | Iter_prem_exit _ -> ()
    | Prem_enter _ | Prem_exit _ -> ()
    | Instr _ -> ()
end

let make cfg =
  config := cfg;
  fmt := Instrumentation_api.Output.formatter cfg.output;
  (module M : Instrumentation_api.Handler.S)

module Spec : Instrumentation_spec.Spec.S = struct
  let name = "tree"
  let mode = `Both

  let params =
    [
      ("level", "LEVEL verbosity level: rules|inputs");
      Instrumentation_spec.Param_utils.output_param;
    ]

  let parse_level = function
    | "rules" -> Rules
    | "inputs" -> Inputs
    | s -> failwith ("Invalid tree level: " ^ s ^ " (expected: rules|inputs)")

  let parse alist =
    match Instrumentation_spec.Param_utils.get alist "level" with
    | None -> None
    | Some s ->
        let output =
          Instrumentation_spec.Param_utils.output_of
            (Instrumentation_spec.Param_utils.get alist "output")
        in
        Some
          {
            Instrumentation_config.Handler_config.name;
            mode;
            handler = make { level = parse_level s; output };
            output;
          }

  let checkpoint = None
end

let spec : Instrumentation_spec.Spec.t = (module Spec)
