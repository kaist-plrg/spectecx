(* IL Node coverage handler - Tracks premise execution.

   Implements Instrumentation_core.Handler.S interface.
   Records all premises at init(), then tracks which are hit during execution.

   Output levels:
   - Summary: stats + uncovered items only
   - Full: GCOV-style annotated spec with execution counts

   Usage:
     let handler = Node_coverage_il.make { level = Full; output = Instrumentation_core.Output.stdout }
*)

open Common.Source
open Lang.Il
open Instrumentation_core.Util
open Instrumentation_static.Premise_uid

(* Verbosity levels *)
type level = Summary | Full

(* Handler configuration *)
type config = { level : level; output : Instrumentation_core.Output.t }

let default_config =
  { level = Summary; output = Instrumentation_core.Output.stdout }

let config = ref default_config
let fmt = ref Format.std_formatter

(* Runtime state - changes during execution *)
module State = struct
  let il_spec : spec ref = ref []
  let prems_attempted : (region * string, int) Hashtbl.t = Hashtbl.create 256
  let prems_succeeded : (region * string, int) Hashtbl.t = Hashtbl.create 256
  let prems_failed : (region * string, int) Hashtbl.t = Hashtbl.create 256

  let prem_to_test : (region * string, string list) Hashtbl.t =
    Hashtbl.create 256

  let current_test_case_id : string option ref = ref None
  let total_prems = ref 0
  let total_fallible_prems = ref 0
  let total_rule_prems = ref 0
  let total_if_prems = ref 0

  let reset () =
    il_spec := [];
    Hashtbl.clear prems_attempted;
    Hashtbl.clear prems_succeeded;
    Hashtbl.clear prems_failed;
    Hashtbl.clear prem_to_test;
    current_test_case_id := None;
    total_prems := 0;
    total_fallible_prems := 0;
    total_rule_prems := 0;
    total_if_prems := 0

  (* Set current test case ID (called by runner before each test) *)
  let set_test_case_id id = current_test_case_id := Some id
  let clear_test_case_id () = current_test_case_id := None

  (* Record that a premise was covered by the current test case *)
  let record_premise_coverage key =
    match !current_test_case_id with
    | Some test_id ->
        let existing =
          Hashtbl.find_opt prem_to_test key |> Option.value ~default:[]
        in
        if not (List.mem test_id existing) then
          Hashtbl.replace prem_to_test key (test_id :: existing)
    | None -> ()

  let incr_count tbl key =
    let count = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
    Hashtbl.replace tbl key (count + 1)
end

let rec is_fallible prem =
  match prem.it with
  | LetPr _ | ElsePr | DebugPr _ -> false
  | IterPr (inner, _) -> is_fallible inner
  | IfPr _ | RulePr _ -> true

module M : Instrumentation_core.Handler.S = struct
  let static_dependencies =
    [
      (module Instrumentation_static.Premise_uid.Premise_uid
      : Instrumentation_static.Static.S);
    ]

  let rec count_prem prem =
    match prem.it with
    (* count IfPr, RulePr, and their iterations *)
    | LetPr _ | ElsePr | DebugPr _ ->
        State.total_prems := !State.total_prems + 1
    | IterPr (inner, _) -> count_prem inner
    | IfPr _ ->
        State.total_prems := !State.total_prems + 1;
        State.total_fallible_prems := !State.total_fallible_prems + 1;
        State.total_if_prems := !State.total_if_prems + 1
    | RulePr _ ->
        State.total_prems := !State.total_prems + 1;
        State.total_fallible_prems := !State.total_fallible_prems + 1;
        State.total_rule_prems := !State.total_rule_prems + 1

  let init ~spec =
    State.reset ();
    match spec with
    | Instrumentation_core.Handler.IlSpec il_spec ->
        State.il_spec := il_spec;
        List.iter
          (fun def ->
            match def.it with
            | RelD (_, _, _, rules) ->
                List.iter
                  (fun rule ->
                    let _, _, prems = rule.it in
                    List.iter (fun prem -> count_prem prem) prems)
                  rules
            | DecD (_, _, _, _, clauses) ->
                List.iter
                  (fun clause ->
                    let _, _, prems = clause.it in
                    List.iter (fun prem -> count_prem prem) prems)
                  clauses
            | TypD _ -> ())
          il_spec
    | Instrumentation_core.Handler.SlSpec _ -> ()

  (* Test lifecycle hooks - manage test case ID for coverage tracking *)
  let on_test_start ~test_case_id:id = State.set_test_case_id id
  let on_test_end ~test_case_id:_ = State.clear_test_case_id ()
  let on_rel_enter = Instrumentation_core.Noop.on_rel_enter
  let on_rel_exit = Instrumentation_core.Noop.on_rel_exit
  let on_rule_enter = Instrumentation_core.Noop.on_rule_enter
  let on_rule_exit = Instrumentation_core.Noop.on_rule_exit
  let on_func_enter = Instrumentation_core.Noop.on_func_enter
  let on_func_exit = Instrumentation_core.Noop.on_func_exit
  let on_clause_enter = Instrumentation_core.Noop.on_clause_enter
  let on_clause_exit = Instrumentation_core.Noop.on_clause_exit
  let on_iter_prem_enter = Instrumentation_core.Noop.on_iter_prem_enter
  let on_iter_prem_exit = Instrumentation_core.Noop.on_iter_prem_exit

  let on_prem_enter ~prem ~at:_ =
    let key = prem_key prem in
    State.incr_count State.prems_attempted key;
    State.record_premise_coverage key

  let on_prem_exit ~prem ~at:_ ~success =
    let key = prem_key prem in
    if success then (
      State.incr_count State.prems_succeeded key;
      State.record_premise_coverage key)
    else
      let rec incr_failures prem =
        match prem.it with
        | LetPr _ | ElsePr | DebugPr _ -> ()
        | IterPr (inner, _) -> incr_failures inner
        | IfPr _ | RulePr _ ->
            let key = prem_key prem in
            State.incr_count State.prems_failed key
      in
      incr_failures prem

  let on_instr = Instrumentation_core.Noop.on_instr

  (* --- Output: Summary mode (stats + uncovered only) --- *)

  let is_if_prem_key ((_, content) : region * string) : bool =
    String.length content >= 3 && String.sub content 0 3 = "if "

  let print_stats () =
    let succeeded = Hashtbl.length State.prems_succeeded in
    let attempted = Hashtbl.length State.prems_attempted in
    let total = !State.total_prems in

    if total > 0 then (
      Format.fprintf !fmt
        "IL Premises: %d/%d attempted (%.2f%%), %d/%d succeeded (%.2f%%)\n"
        attempted total
        (percentage attempted total)
        succeeded total
        (percentage succeeded total);

      (* Breakdown for if-premises *)
      let total_if = !State.total_if_prems in
      let attempted_if =
        Hashtbl.fold
          (fun k _ acc -> if is_if_prem_key k then acc + 1 else acc)
          State.prems_attempted 0
      in
      let failed_if =
        Hashtbl.fold
          (fun k _ acc -> if is_if_prem_key k then acc + 1 else acc)
          State.prems_failed 0
      in
      let never_attempted = total_if - attempted_if in
      let attempted_failed = failed_if in
      let attempted_never_failed = attempted_if - failed_if in

      Format.fprintf !fmt "%d rule premises\n" !State.total_rule_prems;
      Format.fprintf !fmt
        "%d if-premises : %d never attempted, %d attempted but never failed, \
         %d attempted and failed\n"
        total_if never_attempted attempted_never_failed attempted_failed)

  let print_uncovered () =
    let total = !State.total_prems in
    if total > 0 then (
      (* Collect uncovered - premises never successfully executed *)
      let uncovered = ref [] in
      List.iter
        (fun def ->
          match def.it with
          | RelD (id, _, _, rules) ->
              List.iter
                (fun rule ->
                  let rule_id, _, prems = rule.it in
                  List.iter
                    (fun prem ->
                      if not (Hashtbl.mem State.prems_succeeded (prem_key prem))
                      then
                        uncovered :=
                          (id.it, rule_id.it, Print.string_of_prem prem)
                          :: !uncovered)
                    prems)
                rules
          | DecD (id, _, _, _, clauses) ->
              List.iteri
                (fun idx clause ->
                  let _, _, prems = clause.it in
                  List.iter
                    (fun prem ->
                      if not (Hashtbl.mem State.prems_succeeded (prem_key prem))
                      then
                        uncovered :=
                          ( id.it,
                            Format.sprintf "clause/%d" idx,
                            Print.string_of_prem prem )
                          :: !uncovered)
                    prems)
                clauses
          | TypD _ -> ())
        !State.il_spec;
      if !uncovered <> [] then (
        Format.fprintf !fmt "\nNever succeeded:\n";
        List.iter
          (fun (rel, rule, content) ->
            Format.fprintf !fmt "  %s/%s:\n    %s\n" rel rule
              (normalize_whitespace content))
          (List.rev !uncovered)))

  (* --- Output: Full mode (GCOV-style annotated spec) --- *)

  (* Format as succ/fail - omit fail for let premises *)
  let fmt_succ_fail prem =
    let key = prem_key prem in
    let succ =
      Hashtbl.find_opt State.prems_succeeded key |> Option.value ~default:0
    in
    let succ_str = format_count succ in
    match prem.it with
    | LetPr _ -> Format.sprintf "%s     " succ_str
    | _ ->
        let fail =
          Hashtbl.find_opt State.prems_failed key |> Option.value ~default:0
        in
        let fail_str = format_count fail in
        Format.sprintf "%s/%s" succ_str fail_str

  let get_prem_succeeded key =
    Hashtbl.find_opt State.prems_succeeded key |> Option.value ~default:0

  let print_prem indent prem =
    let content = Print.string_of_prem prem |> normalize_whitespace in
    let uid =
      match get_uid (prem_key prem) with Some uid -> uid | None -> -1
    in
    if is_fallible prem then
      let succ_fail = fmt_succ_fail prem in
      Format.fprintf !fmt "%4d: %s %s-- %s\n" uid succ_fail indent content
    else
      let succ = get_prem_succeeded (prem_key prem) in
      Format.fprintf !fmt "%4d: %s     %s-- %s\n" uid (format_count succ) indent
        content

  let print_prems indent prems =
    List.iter (print_prem indent) prems;
    (* Print success count for final premise *)
    match List.rev prems with
    | last :: _ ->
        let key = prem_key last in
        let succ = get_prem_succeeded key in
        Format.fprintf !fmt "      %s      %sSUCCESS\n" (format_count succ)
          indent
    | [] -> ()

  let print_full () =
    List.iter
      (fun def ->
        match def.it with
        | RelD (id, _, _, rules) ->
            Format.fprintf !fmt "\nrelation %s:\n" id.it;
            List.iter
              (fun rule ->
                let rule_id, _, prems = rule.it in
                Format.fprintf !fmt "      rule %s:\n" rule_id.it;
                print_prems "    " prems)
              rules
        | DecD (id, _, _, _, clauses) ->
            Format.fprintf !fmt "\ndef $%s:\n" id.it;
            List.iteri
              (fun idx clause ->
                let _, _, prems = clause.it in
                Format.fprintf !fmt "      clause %d:\n" idx;
                print_prems "    " prems)
              clauses
        | TypD _ -> ())
      !State.il_spec

  (* --- Finish: print report --- *)

  let finish () =
    if !State.total_prems > 0 then (
      Format.fprintf !fmt "\n=== IL Node Coverage ===\n\n";
      match !config.level with
      | Summary ->
          print_stats ();
          print_uncovered ()
      | Full ->
          print_stats ();
          print_full ())
end

(* Result type for programmatic access *)
type result = {
  prems_attempted : ((region * string) * int) list; (* key * count *)
  prems_succeeded : ((region * string) * int) list; (* key * count *)
  prem_to_uid : ((region * string) * int) list; (* key * uid *)
  uid_to_prem : (int * (region * string)) list; (* uid * key *)
  prem_to_test : ((region * string) * string list) list;
      (* key * test_case_ids *)
  total_prems : int;
}

let get_result () =
  let prem_to_uid_list, uid_to_prem_list =
    match Instrumentation_static.Premise_uid.export () with
    | Some (prem_to_uid, uid_to_prem) -> (prem_to_uid, uid_to_prem)
    | None -> ([], [])
  in
  {
    prems_attempted = State.prems_attempted |> Hashtbl.to_seq |> List.of_seq;
    prems_succeeded = State.prems_succeeded |> Hashtbl.to_seq |> List.of_seq;
    prem_to_uid = prem_to_uid_list;
    uid_to_prem = uid_to_prem_list;
    prem_to_test = State.prem_to_test |> Hashtbl.to_seq |> List.of_seq;
    total_prems = !State.total_prems;
  }

(* Restore state from a previous result (for checkpoint resume) *)
let restore result =
  Hashtbl.clear State.prems_attempted;
  Hashtbl.clear State.prems_succeeded;
  Hashtbl.clear State.prems_failed;
  Hashtbl.clear State.prem_to_test;
  List.iter
    (fun (key, count) -> Hashtbl.replace State.prems_attempted key count)
    result.prems_attempted;
  List.iter
    (fun (key, count) -> Hashtbl.replace State.prems_succeeded key count)
    result.prems_succeeded;
  (* Restore UID mapping through static service *)
  Instrumentation_static.Premise_uid.restore
    (result.prem_to_uid, result.uid_to_prem);
  (* Reconstruct prems_failed from attempted - succeeded *)
  List.iter
    (fun (key, attempted_count) ->
      let succeeded_count =
        Hashtbl.find_opt State.prems_succeeded key |> Option.value ~default:0
      in
      let failed_count = attempted_count - succeeded_count in
      if failed_count > 0 then
        Hashtbl.replace State.prems_failed key failed_count)
    result.prems_attempted;
  List.iter
    (fun (key, test_cases) -> Hashtbl.replace State.prem_to_test key test_cases)
    result.prem_to_test;
  State.total_prems := result.total_prems

(* Expose test case ID setter for use by runner *)
let set_test_case_id = State.set_test_case_id
let clear_test_case_id = State.clear_test_case_id

(* Handler with data access - implements HANDLER_WITH_DATA signature *)
module HandlerWithData :
  Instrumentation_core.Handler.S_with_data with type result = result = struct
  include M

  type nonrec result = result

  let get_result = get_result
  let restore = restore
end

let make cfg =
  config := cfg;
  fmt := Instrumentation_core.Output.formatter cfg.output;
  (module M : Instrumentation_core.Handler.S)

(* Create handler with data getter for programmatic access.
   Usage:
     let handler, get_coverage = Node_coverage.make_with_data cfg in
     Hooks.set_handlers [handler];
     (* ... run interpreter ... *)
     let data = get_coverage () in
*)
let make_with_data cfg =
  config := cfg;
  fmt := Instrumentation_core.Output.formatter cfg.output;
  ( (module HandlerWithData : Instrumentation_core.Handler.S_with_data
      with type result = result),
    get_result )
