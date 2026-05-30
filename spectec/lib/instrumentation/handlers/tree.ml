(** Tree: buffered derivation-tree renderer for a successful evaluation.

    Unlike {!Trace} (which streams every event), Tree accumulates events inside
    a top-level relation invocation and emits a single ASCII tree once that
    invocation completes. Failed rule attempts are pruned: only the rule that
    actually fired at each relation invocation remains visible.

    Each relation is drawn as a derivation tree in spec syntax: its conclusion
    on top, its sub-derivations below as premises led by [--].

    Levels:
    - [Rules]: each relation node tagged with the rule that matched.
    - [Conclusion]: above + the conclusion judgment under each tag (e.g.
      [[ x -> int ] |- 5 : int]); functions show [$id(args) = output]. *)

module Il = Lang.Il
open Util

type level = Rules | Conclusion
type config = { level : level; output : Instrumentation_api.Output.t }

let default_config =
  { level = Rules; output = Instrumentation_api.Output.stdout }

let config = ref default_config
let fmt = ref Format.std_formatter

let summarize_value (v : Il.Value.t) : string =
  Il.Print.string_of_value v |> summarize ~max_len:100

(* === Tree representation =========================================== *)

type kind = Rel | Func

type outcome =
  | Failed
  | Rel_ok of (Il.Value.t, Il.Value.t) Il.Mode.t
  | Func_ok of Il.Value.t

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

let outcome_of_conclusion = function Some c -> Rel_ok c | None -> Failed
let outcome_of_output = function Some v -> Func_ok v | None -> Failed

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

let render_judgment c =
  Il.Mode.render ~pad_brackets:true ~string_of_atom:Il.Print.string_of_atom
    ~string_of_arg:summarize_value c

let render_call node =
  let args = List.map summarize_value node.inputs |> String.concat ", " in
  Format.sprintf "$%s(%s)" node.id args

let render_tag node =
  match node.rule with Some r when r <> "" -> node.id ^ "/" ^ r | _ -> node.id

(* Count code points, not bytes, so the bar matches the conclusion's width. *)
let measure_width s =
  String.fold_left
    (fun width c -> if Char.code c land 0xc0 = 0x80 then width else width + 1)
    0 s

(* Box-drawing glyph so that the bar consistently renders connected. *)
let render_bar n = String.concat "" (List.init n (fun _ -> "─"))

let render_lines node =
  match (node.kind, !config.level, node.outcome) with
  | Rel, Conclusion, Rel_ok c ->
      let notation = render_judgment c in
      [ render_tag node ^ ":"; notation; render_bar (measure_width notation) ]
  | Rel, _, _ -> [ render_tag node ]
  | Func, Conclusion, Func_ok v ->
      [ Format.sprintf "%s = %s" (render_call node) (summarize_value v) ]
  | Func, _, _ -> [ "$" ^ node.id ]

let rec print_node ~first_lead ~rest_prefix node out =
  (match render_lines node with
  | [] -> ()
  | head :: rest ->
      Format.fprintf out "%s%s\n" first_lead head;
      List.iter (fun l -> Format.fprintf out "%s%s\n" rest_prefix l) rest);
  let child_lead = rest_prefix ^ "-- " in
  let child_rest = rest_prefix ^ "   " in
  List.iter
    (fun c -> print_node ~first_lead:child_lead ~rest_prefix:child_rest c out)
    (List.rev node.children_rev)

let print_root node =
  print_node ~first_lead:"" ~rest_prefix:"" node !fmt;
  Format.pp_print_flush !fmt ()

let close_and_maybe_print ~outcome =
  match State.close_top ~outcome with
  | Some { outcome = Failed; _ } | None -> ()
  | Some root -> print_root root

(* === Handler module ================================================ *)

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []
  let init ~spec:_ = State.reset ()
  let finish () = ()

  let handle : Instrumentation_api.Event.t -> unit = function
    | Test_start _ | Test_end _ -> State.reset ()
    | Rel_enter { id; at = _; inputs } -> State.push (new_node Rel id inputs)
    | Rel_exit { id = _; at = _; conclusion } ->
        close_and_maybe_print ~outcome:(outcome_of_conclusion conclusion)
    | Rule_enter _ -> State.begin_rule_attempt ()
    | Rule_exit { id = _; rule_id; at = _; success } ->
        State.end_rule_attempt ~rule_id ~success
    | Func_enter { id; at = _; inputs } -> State.push (new_node Func id inputs)
    | Func_exit { id = _; at = _; output } ->
        close_and_maybe_print ~outcome:(outcome_of_output output)
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
      ("level", "LEVEL verbosity level: rules|conclusion");
      Instrumentation_spec.Param_utils.output_param;
    ]

  let parse_level = function
    | "rules" -> Rules
    | "conclusion" -> Conclusion
    | s ->
        failwith ("Invalid tree level: " ^ s ^ " (expected: rules|conclusion)")

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
