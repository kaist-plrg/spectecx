(** [Summary] lists instructions that never succeeded. [Full] annotates the spec
    with attempts and successes and counts failures for [if] and relation
    instructions. *)

open Common.Annotated
module Location = Common.Location
open Lang.Al
module Il = Lang.Il
open Util
open Instrumentation_static.Instr_uid

type level = Summary | Full
type config = { level : level; output : Instrumentation_api.Output.t }

let default_config =
  { level = Summary; output = Instrumentation_api.Output.stdout }

let config = ref default_config
let fmt = ref Format.std_formatter

(* Runtime state - changes during execution *)
module State = struct
  let al_spec : spec ref = ref []

  let instrs_attempted : (Location.t * string, int) Hashtbl.t =
    Hashtbl.create 256

  let instrs_succeeded : (Location.t * string, int) Hashtbl.t =
    Hashtbl.create 256

  let instrs_failed : (Location.t * string, int) Hashtbl.t = Hashtbl.create 256

  let instr_to_test : (Location.t * string, string list) Hashtbl.t =
    Hashtbl.create 256

  let current_test_case_id : string option ref = ref None
  let total_instrs = ref 0
  let total_fallible_instrs = ref 0
  let total_rel_instrs = ref 0
  let total_if_instrs = ref 0

  let reset () =
    al_spec := [];
    Hashtbl.clear instrs_attempted;
    Hashtbl.clear instrs_succeeded;
    Hashtbl.clear instrs_failed;
    Hashtbl.clear instr_to_test;
    current_test_case_id := None;
    total_instrs := 0;
    total_fallible_instrs := 0;
    total_rel_instrs := 0;
    total_if_instrs := 0

  (* Set current test case ID (called by runner before each test) *)
  let set_test_case_id id = current_test_case_id := Some id
  let clear_test_case_id () = current_test_case_id := None

  let record_instr_coverage key =
    match !current_test_case_id with
    | Some test_id ->
        let existing =
          Hashtbl.find_opt instr_to_test key |> Option.value ~default:[]
        in
        if not (List.mem test_id existing) then
          Hashtbl.replace instr_to_test key (test_id :: existing)
    | None -> ()

  let incr_count tbl key =
    let count = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
    Hashtbl.replace tbl key (count + 1)
end

let rec is_fallible instr =
  match instr.it with
  | LetI _ | OtherwiseI | DebugI _ | RelAssertI _ -> false
  | IterI (inner, _) -> is_fallible inner
  | IfI _ | RelI _ -> true

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies =
    [
      (module Instrumentation_static.Instr_uid.Instr_uid
      : Instrumentation_static.Static.S);
    ]

  let rec count_instr instr =
    match instr.it with
    | LetI _ | OtherwiseI | DebugI _ | RelAssertI _ ->
        State.total_instrs := !State.total_instrs + 1
    | IterI (inner, _) -> count_instr inner
    | IfI _ ->
        State.total_instrs := !State.total_instrs + 1;
        State.total_fallible_instrs := !State.total_fallible_instrs + 1;
        State.total_if_instrs := !State.total_if_instrs + 1
    | RelI _ ->
        State.total_instrs := !State.total_instrs + 1;
        State.total_fallible_instrs := !State.total_fallible_instrs + 1;
        State.total_rel_instrs := !State.total_rel_instrs + 1

  let init ~spec =
    State.reset ();
    match spec with
    | Instrumentation_api.Handler.AlSpec al_spec ->
        State.al_spec := al_spec;
        List.iter
          (fun def ->
            match def.it with
            | RelD { rules; _ } ->
                List.iter
                  (fun rule ->
                    let ({ instrs; _ } : rule') = rule.it in
                    List.iter (fun instr -> count_instr instr) instrs)
                  rules
            | DecD { clauses; _ } ->
                List.iter
                  (fun clause ->
                    let { instrs; _ } = clause.it in
                    List.iter (fun instr -> count_instr instr) instrs)
                  clauses
            | _ -> ())
          al_spec
    | Instrumentation_api.Handler.SlSpec _ -> ()

  let handle_value = None

  let handle : Instrumentation_api.Event.t -> unit = function
    | Test_start { test_case_id } -> State.set_test_case_id test_case_id
    | Test_end _ -> State.clear_test_case_id ()
    | Instr_enter { instr = AlInstr instr; at = _ } ->
        let key = instr_key instr in
        State.incr_count State.instrs_attempted key;
        State.record_instr_coverage key
    | Instr_exit { instr = AlInstr instr; at = _; success; bindings = _ } ->
        let key = instr_key instr in
        if success then (
          State.incr_count State.instrs_succeeded key;
          State.record_instr_coverage key)
        else
          let rec incr_failures instr =
            match instr.it with
            | LetI _ | OtherwiseI | DebugI _ | RelAssertI _ -> ()
            | IterI (inner, _) -> incr_failures inner
            | IfI _ | RelI _ ->
                let key = instr_key instr in
                State.incr_count State.instrs_failed key
          in
          incr_failures instr
    | _ -> ()

  (* --- Output: Summary mode (stats + uncovered only) --- *)

  let is_if_instr_key ((_, content) : Location.t * string) : bool =
    String.length content >= 3 && String.sub content 0 3 = "if "

  let print_stats () =
    let succeeded = Hashtbl.length State.instrs_succeeded in
    let attempted = Hashtbl.length State.instrs_attempted in
    let total = !State.total_instrs in

    if total > 0 then (
      Format.fprintf !fmt
        "AL Instructions: %d/%d attempted (%.2f%%), %d/%d succeeded (%.2f%%)\n"
        attempted total
        (percentage attempted total)
        succeeded total
        (percentage succeeded total);

      let total_if = !State.total_if_instrs in
      let succeeded_if =
        Hashtbl.fold
          (fun k _ acc -> if is_if_instr_key k then acc + 1 else acc)
          State.instrs_succeeded 0
      in
      let failed_if =
        Hashtbl.fold
          (fun k _ acc -> if is_if_instr_key k then acc + 1 else acc)
          State.instrs_failed 0
      in
      let both_if =
        Hashtbl.fold
          (fun k _ acc ->
            if is_if_instr_key k && Hashtbl.mem State.instrs_succeeded k then
              acc + 1
            else acc)
          State.instrs_failed 0
      in
      let neither_if = total_if - (succeeded_if + failed_if - both_if) in
      let total_score = succeeded_if + failed_if in
      let twice_total_if = 2 * total_if in

      Format.fprintf !fmt "%d rel instructions\n" !State.total_rel_instrs;
      Format.fprintf !fmt
        "%d if instructions: succeeded %d/%d (%.2f%%), failed %d/%d (%.2f%%), \
         neither %d/%d (%.2f%%), total %d/%d (%.2f%%)\n"
        total_if succeeded_if total_if
        (percentage succeeded_if total_if)
        failed_if total_if
        (percentage failed_if total_if)
        neither_if total_if
        (percentage neither_if total_if)
        total_score twice_total_if
        (percentage total_score twice_total_if))

  let print_uncovered () =
    let total = !State.total_instrs in
    if total > 0 then (
      let uncovered = ref [] in
      List.iter
        (fun def ->
          match def.it with
          | RelD { relid = id; rules; _ } ->
              List.iter
                (fun rule ->
                  let ({ ruleid = rule_id; instrs; _ } : rule') = rule.it in
                  List.iter
                    (fun instr ->
                      if
                        not
                          (Hashtbl.mem State.instrs_succeeded (instr_key instr))
                      then
                        uncovered :=
                          (id.it, rule_id.it, Print.string_of_instr instr)
                          :: !uncovered)
                    instrs)
                rules
          | DecD { defid = id; clauses; _ } ->
              List.iteri
                (fun idx clause ->
                  let { instrs; _ } = clause.it in
                  List.iter
                    (fun instr ->
                      if
                        not
                          (Hashtbl.mem State.instrs_succeeded (instr_key instr))
                      then
                        uncovered :=
                          ( id.it,
                            Format.sprintf "clause/%d" idx,
                            Print.string_of_instr instr )
                          :: !uncovered)
                    instrs)
                clauses
          | _ -> ())
        !State.al_spec;
      if !uncovered <> [] then (
        Format.fprintf !fmt "\nNever succeeded:\n";
        List.iter
          (fun (rel, rule, content) ->
            Format.fprintf !fmt "  %s/%s:\n    %s\n" rel rule
              (normalize_whitespace content))
          (List.rev !uncovered)))

  (* --- Output: Full mode (GCOV-style annotated spec) --- *)

  let fmt_succ_fail instr =
    let key = instr_key instr in
    let succ =
      Hashtbl.find_opt State.instrs_succeeded key |> Option.value ~default:0
    in
    let succ_str = format_count succ in
    match instr.it with
    | LetI _ -> Format.sprintf "%s     " succ_str
    | _ ->
        let fail =
          Hashtbl.find_opt State.instrs_failed key |> Option.value ~default:0
        in
        let fail_str = format_count fail in
        Format.sprintf "%s/%s" succ_str fail_str

  let get_instr_succeeded key =
    Hashtbl.find_opt State.instrs_succeeded key |> Option.value ~default:0

  let print_instr indent instr =
    let content = Print.string_of_instr instr |> normalize_whitespace in
    let uid =
      match get_uid (instr_key instr) with Some uid -> uid | None -> -1
    in
    if is_fallible instr then
      let succ_fail = fmt_succ_fail instr in
      Format.fprintf !fmt "%4d: %s %s-- %s\n" uid succ_fail indent content
    else
      let succ = get_instr_succeeded (instr_key instr) in
      Format.fprintf !fmt "%4d: %s     %s-- %s\n" uid (format_count succ) indent
        content

  let print_instrs indent result_str instrs =
    List.iter (print_instr indent) instrs;
    match List.rev instrs with
    | last :: _ ->
        let key = instr_key last in
        let succ = get_instr_succeeded key in
        Format.fprintf !fmt "      %s      %s%s\n" (format_count succ) indent
          result_str
    | [] -> ()

  let print_full () =
    List.iter
      (fun def ->
        match def.it with
        | RelD { relid = id; rules; _ } ->
            Format.fprintf !fmt "\nrelation %s:\n" id.it;
            List.iter
              (fun rule ->
                let ({ ruleid = rule_id; concl; instrs; _ } : rule') =
                  rule.it
                in
                let result_str =
                  Il.Print.string_of_notexp concl |> normalize_whitespace
                in
                Format.fprintf !fmt "      rule %s:\n" rule_id.it;
                print_instrs "    " result_str instrs)
              rules
        | DecD { defid = id; clauses; _ } ->
            Format.fprintf !fmt "\ndef $%s:\n" id.it;
            List.iteri
              (fun idx clause ->
                let { body; instrs; _ } = clause.it in
                let result_str =
                  Il.Print.string_of_exp body |> normalize_whitespace
                in
                Format.fprintf !fmt "      clause %d:\n" idx;
                print_instrs "    " result_str instrs)
              clauses
        | _ -> ())
      !State.al_spec

  (* --- Finish: print report --- *)

  let finish () =
    if !State.total_instrs > 0 then (
      Format.fprintf !fmt "\n=== AL Instruction Coverage ===\n\n";
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
  instrs_attempted : ((Location.t * string) * int) list; (* key * count *)
  instrs_succeeded : ((Location.t * string) * int) list; (* key * count *)
  instr_to_uid : ((Location.t * string) * int) list; (* key * uid *)
  uid_to_instr : (int * (Location.t * string)) list; (* uid * key *)
  instr_to_test : ((Location.t * string) * string list) list;
      (* key * test_case_ids *)
  total_instrs : int;
}

let get_result () =
  let instr_to_uid_list, uid_to_instr_list =
    match Instrumentation_static.Instr_uid.export () with
    | Some (instr_to_uid, uid_to_instr) -> (instr_to_uid, uid_to_instr)
    | None -> ([], [])
  in
  {
    instrs_attempted = State.instrs_attempted |> Hashtbl.to_seq |> List.of_seq;
    instrs_succeeded = State.instrs_succeeded |> Hashtbl.to_seq |> List.of_seq;
    instr_to_uid = instr_to_uid_list;
    uid_to_instr = uid_to_instr_list;
    instr_to_test = State.instr_to_test |> Hashtbl.to_seq |> List.of_seq;
    total_instrs = !State.total_instrs;
  }

(* Restore state from a previous result (for checkpoint resume) *)
let restore result =
  Hashtbl.clear State.instrs_attempted;
  Hashtbl.clear State.instrs_succeeded;
  Hashtbl.clear State.instrs_failed;
  Hashtbl.clear State.instr_to_test;
  List.iter
    (fun (key, count) -> Hashtbl.replace State.instrs_attempted key count)
    result.instrs_attempted;
  List.iter
    (fun (key, count) -> Hashtbl.replace State.instrs_succeeded key count)
    result.instrs_succeeded;
  (* Restore UID mapping through static service *)
  Instrumentation_static.Instr_uid.restore
    (result.instr_to_uid, result.uid_to_instr);
  List.iter
    (fun (key, attempted_count) ->
      let succeeded_count =
        Hashtbl.find_opt State.instrs_succeeded key |> Option.value ~default:0
      in
      let failed_count = attempted_count - succeeded_count in
      if failed_count > 0 then
        Hashtbl.replace State.instrs_failed key failed_count)
    result.instrs_attempted;
  List.iter
    (fun (key, test_cases) ->
      Hashtbl.replace State.instr_to_test key test_cases)
    result.instr_to_test;
  State.total_instrs := result.total_instrs

(* Merge two results — used for checkpoint merging *)
let merge_results r1 r2 =
  let merge_counts counts1 counts2 =
    let tbl = Hashtbl.create 256 in
    let add key count =
      let existing = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
      Hashtbl.replace tbl key (existing + count)
    in
    List.iter (fun (key, count) -> add key count) counts1;
    List.iter (fun (key, count) -> add key count) counts2;
    Hashtbl.to_seq tbl |> List.of_seq
  in
  let merge_test_lists tests1 tests2 =
    let tbl = Hashtbl.create 256 in
    let union_test_ids existing new_ids =
      List.fold_left
        (fun existing test_id ->
          if List.mem test_id existing then existing else test_id :: existing)
        existing new_ids
    in
    let add key test_ids =
      let existing = Hashtbl.find_opt tbl key |> Option.value ~default:[] in
      Hashtbl.replace tbl key (union_test_ids existing test_ids)
    in
    List.iter (fun (key, test_ids) -> add key test_ids) tests1;
    List.iter (fun (key, test_ids) -> add key test_ids) tests2;
    Hashtbl.to_seq tbl |> List.of_seq
  in
  {
    instr_to_uid = r1.instr_to_uid;
    uid_to_instr = r1.uid_to_instr;
    total_instrs = r1.total_instrs;
    instrs_attempted = merge_counts r1.instrs_attempted r2.instrs_attempted;
    instrs_succeeded = merge_counts r1.instrs_succeeded r2.instrs_succeeded;
    instr_to_test = merge_test_lists r1.instr_to_test r2.instr_to_test;
  }

let make cfg =
  config := cfg;
  fmt := Instrumentation_api.Output.formatter cfg.output;
  (module M : Instrumentation_api.Handler.S)
