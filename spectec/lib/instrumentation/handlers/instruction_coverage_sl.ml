(** [Summary] lists every instruction and case that never succeeded. [Full]
    annotates the spec with counts, including failed checks and matches or
    failures of individual cases. *)

open Common.Annotated
module Location = Common.Location
module Sl = Lang.Sl
open Util

type level = Instruction_coverage_al.level = Summary | Full

type config = Instruction_coverage_al.config = {
  level : level;
  output : Instrumentation_api.Output.t;
}

let default_config = Instruction_coverage_al.default_config
let config = ref default_config
let fmt = ref Format.std_formatter

(* Runtime state - changes during execution *)
module State = struct
  let sl_spec : Sl.spec ref = ref []

  let instrs_attempted : (Location.t * string, int) Hashtbl.t =
    Hashtbl.create 256

  let instrs_succeeded : (Location.t * string, int) Hashtbl.t =
    Hashtbl.create 256

  let instrs_failed : (Location.t * string, int) Hashtbl.t = Hashtbl.create 256

  let cases_matched : ((Location.t * string) * int, int) Hashtbl.t =
    Hashtbl.create 64

  let cases_failed : ((Location.t * string) * int, int) Hashtbl.t =
    Hashtbl.create 64

  let fallible_keys : (Location.t * string, unit) Hashtbl.t = Hashtbl.create 256
  let total_instrs = ref 0
  let total_rel_instrs = ref 0
  let total_fallible_instrs = ref 0

  let reset () =
    sl_spec := [];
    Hashtbl.clear instrs_attempted;
    Hashtbl.clear instrs_succeeded;
    Hashtbl.clear instrs_failed;
    Hashtbl.clear cases_matched;
    Hashtbl.clear cases_failed;
    Hashtbl.clear fallible_keys;
    total_instrs := 0;
    total_rel_instrs := 0;
    total_fallible_instrs := 0

  let incr_count tbl key =
    let count = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
    Hashtbl.replace tbl key (count + 1)
end

let instr_header instr = Sl.Print.string_of_instr ~short:true instr

(* SL instructions have no identifiers, so coverage uses their locations and
   headers (the printed instructions without their child blocks). *)
let instr_key instr =
  let content = instr_header instr |> normalize_whitespace in
  (instr.at, content)

let is_fallible instr =
  match instr.it with
  | Sl.IfI _ | Sl.RelAssertI _ | Sl.CaseI _ -> true
  | Sl.RelI _ | Sl.LetI _ | Sl.ResultI _ | Sl.ReturnI _ | Sl.DebugI _ -> false

let blocks_of_instr instr =
  match instr.it with
  | Sl.RelI { block; _ } | Sl.RelAssertI { block; _ } | Sl.LetI (_, _, _, block)
    ->
      [ block ]
  | Sl.IfI (_, _, instrs, _) -> [ instrs ]
  | Sl.CaseI (_, cases, _) -> List.map snd cases
  | Sl.DebugI (_, inner) -> [ [ inner ] ]
  | Sl.ResultI _ | Sl.ReturnI _ -> []

let blocks_of_def def =
  match def.it with
  | Sl.RelD (id, _, block, elseblock_opt) ->
      Some (id, block :: Option.to_list elseblock_opt)
  | Sl.DecD (id, _, _, block, elseblock_opt) ->
      Some (id, block :: Option.to_list elseblock_opt)
  | Sl.TypD _ | Sl.BuiltinDecD _ -> None

let rec iter_instr_preorder ~f instr =
  f instr;
  List.iter (List.iter (iter_instr_preorder ~f)) (blocks_of_instr instr)

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []

  let count_instr instr =
    incr State.total_instrs;
    if is_fallible instr then (
      incr State.total_fallible_instrs;
      Hashtbl.replace State.fallible_keys (instr_key instr) ());
    match instr.it with Sl.RelI _ -> incr State.total_rel_instrs | _ -> ()

  let init ~spec =
    State.reset ();
    match spec with
    | Instrumentation_api.Handler.AlSpec _ -> ()
    | Instrumentation_api.Handler.SlSpec sl_spec ->
        State.sl_spec := sl_spec;
        List.iter
          (fun def ->
            Option.iter
              (fun (_, blocks) ->
                List.iter
                  (List.iter (iter_instr_preorder ~f:count_instr))
                  blocks)
              (blocks_of_def def))
          sl_spec

  let handle_value = None

  let handle : Instrumentation_api.Event.t -> unit = function
    | Instr_enter { instr = SlInstr instr; at = _ } ->
        State.incr_count State.instrs_attempted (instr_key instr)
    | Instr_exit { instr = SlInstr instr; at = _; success; bindings = _ } ->
        let key = instr_key instr in
        if success then State.incr_count State.instrs_succeeded key
        else if is_fallible instr then State.incr_count State.instrs_failed key
    | Case_match { instr = SlInstr instr; at = _; case_idx } -> (
        let key = instr_key instr in
        let failed_before n =
          List.iter
            (fun idx -> State.incr_count State.cases_failed (key, idx))
            (List.init n Fun.id)
        in
        match (case_idx, instr.it) with
        | Some idx, _ ->
            State.incr_count State.cases_matched (key, idx);
            failed_before idx
        | None, Sl.CaseI (_, cases, _) -> failed_before (List.length cases)
        | None, _ -> ())
    | _ -> ()

  let print_stats () =
    let attempted = Hashtbl.length State.instrs_attempted in
    let succeeded = Hashtbl.length State.instrs_succeeded in
    let total = !State.total_instrs in
    if total > 0 then (
      Format.fprintf !fmt
        "SL Instructions: %d/%d attempted (%.2f%%), %d/%d succeeded (%.2f%%)\n"
        attempted total
        (percentage attempted total)
        succeeded total
        (percentage succeeded total);
      let total_fallible = !State.total_fallible_instrs in
      let count_fallible tbl =
        Hashtbl.fold
          (fun key _ count ->
            if Hashtbl.mem State.fallible_keys key then count + 1 else count)
          tbl 0
      in
      let succeeded_fallible = count_fallible State.instrs_succeeded in
      let failed_fallible = count_fallible State.instrs_failed in
      let both_fallible =
        Hashtbl.fold
          (fun key _ count ->
            if Hashtbl.mem State.instrs_succeeded key then count + 1 else count)
          State.instrs_failed 0
      in
      let neither_fallible =
        total_fallible - (succeeded_fallible + failed_fallible - both_fallible)
      in
      let total_score = succeeded_fallible + failed_fallible in
      let twice_total_fallible = 2 * total_fallible in
      Format.fprintf !fmt "%d rel instructions\n" !State.total_rel_instrs;
      Format.fprintf !fmt
        "%d if and case instructions: succeeded %d/%d (%.2f%%), failed %d/%d \
         (%.2f%%), neither %d/%d (%.2f%%), total %d/%d (%.2f%%)\n"
        total_fallible succeeded_fallible total_fallible
        (percentage succeeded_fallible total_fallible)
        failed_fallible total_fallible
        (percentage failed_fallible total_fallible)
        neither_fallible total_fallible
        (percentage neither_fallible total_fallible)
        total_score twice_total_fallible
        (percentage total_score twice_total_fallible))

  let print_never_succeeded () =
    let never_succeeded = ref [] in
    let add id content = never_succeeded := (id, content) :: !never_succeeded in
    let rec visit id instr =
      let key = instr_key instr in
      if not (Hashtbl.mem State.instrs_succeeded key) then
        add id (instr_header instr);
      match instr.it with
      | Sl.CaseI (_, cases, _) ->
          List.iteri
            (fun case_idx (guard, instrs) ->
              if not (Hashtbl.mem State.cases_matched (key, case_idx)) then
                add id ("Case " ^ Sl.Print.string_of_guard guard);
              List.iter (visit id) instrs)
            cases
      | _ -> List.iter (List.iter (visit id)) (blocks_of_instr instr)
    in
    List.iter
      (fun def ->
        Option.iter
          (fun ((id : Sl.id), blocks) ->
            List.iter (List.iter (visit id.it)) blocks)
          (blocks_of_def def))
      !State.sl_spec;
    if !never_succeeded <> [] then (
      Format.fprintf !fmt "\nNever succeeded:\n";
      List.iter
        (fun (id, content) ->
          Format.fprintf !fmt "  %s:\n    %s\n" id
            (normalize_whitespace content))
        (List.rev !never_succeeded))

  (* --- Output: Full mode (GCOV-style annotated spec) --- *)

  let count_of tbl key = Hashtbl.find_opt tbl key |> Option.value ~default:0

  let fmt_counts instr =
    let key = instr_key instr in
    let succeeded = format_count (count_of State.instrs_succeeded key) in
    if is_fallible instr then
      Format.sprintf "%s/%s" succeeded
        (format_count (count_of State.instrs_failed key))
    else Format.sprintf "%s     " succeeded

  let rec print_instr indent instr =
    let max_len = max 40 (80 - String.length indent) in
    let content = instr_header instr |> summarize ~max_len in
    Format.fprintf !fmt "%s %s%s\n" (fmt_counts instr) indent content;
    match instr.it with
    | Sl.CaseI (_, cases, _) ->
        let key = instr_key instr in
        List.iteri
          (fun case_idx (guard, instrs) ->
            Format.fprintf !fmt "%s/%s  %s  Case %s:\n"
              (format_count (count_of State.cases_matched (key, case_idx)))
              (format_count (count_of State.cases_failed (key, case_idx)))
              indent
              (Sl.Print.string_of_guard guard);
            List.iter (print_instr (indent ^ "    ")) instrs)
          cases
    | _ ->
        List.iter
          (List.iter (print_instr (indent ^ "  ")))
          (blocks_of_instr instr)

  let print_full () =
    List.iter
      (fun def ->
        match def.it with
        | Sl.RelD (id, _, block, elseblock_opt) ->
            Format.fprintf !fmt "\nrelation %s:\n" id.it;
            List.iter (print_instr "  ") block;
            Option.iter (List.iter (print_instr "  ")) elseblock_opt
        | Sl.DecD (id, _, _, block, elseblock_opt) ->
            Format.fprintf !fmt "\ndef $%s:\n" id.it;
            List.iter (print_instr "  ") block;
            Option.iter (List.iter (print_instr "  ")) elseblock_opt
        | Sl.TypD _ | Sl.BuiltinDecD _ -> ())
      !State.sl_spec

  (* --- Finish: print report --- *)

  let finish () =
    if !State.total_instrs > 0 then (
      Format.fprintf !fmt "\n=== SL Instruction Coverage ===\n\n";
      match !config.level with
      | Summary ->
          print_stats ();
          print_never_succeeded ()
      | Full ->
          print_stats ();
          print_full ())
end

(* Result type for programmatic access *)
type result = {
  instrs_attempted : ((Location.t * string) * int) list; (* key * count *)
  instrs_succeeded : ((Location.t * string) * int) list; (* key * count *)
  instrs_failed : ((Location.t * string) * int) list; (* key * count *)
  cases_matched : (((Location.t * string) * int) * int) list;
  cases_failed : (((Location.t * string) * int) * int) list;
  total_instrs : int;
}

let get_result () =
  let to_list tbl = tbl |> Hashtbl.to_seq |> List.of_seq in
  {
    instrs_attempted = to_list State.instrs_attempted;
    instrs_succeeded = to_list State.instrs_succeeded;
    instrs_failed = to_list State.instrs_failed;
    cases_matched = to_list State.cases_matched;
    cases_failed = to_list State.cases_failed;
    total_instrs = !State.total_instrs;
  }

(* Restore state from a previous result (for checkpoint resume) *)
let restore result =
  let restore_counts tbl counts =
    Hashtbl.clear tbl;
    List.iter (fun (key, count) -> Hashtbl.replace tbl key count) counts
  in
  restore_counts State.instrs_attempted result.instrs_attempted;
  restore_counts State.instrs_succeeded result.instrs_succeeded;
  restore_counts State.instrs_failed result.instrs_failed;
  restore_counts State.cases_matched result.cases_matched;
  restore_counts State.cases_failed result.cases_failed;
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
  {
    instrs_attempted = merge_counts r1.instrs_attempted r2.instrs_attempted;
    instrs_succeeded = merge_counts r1.instrs_succeeded r2.instrs_succeeded;
    instrs_failed = merge_counts r1.instrs_failed r2.instrs_failed;
    cases_matched = merge_counts r1.cases_matched r2.cases_matched;
    cases_failed = merge_counts r1.cases_failed r2.cases_failed;
    total_instrs = r1.total_instrs;
  }

let make cfg =
  config := cfg;
  fmt := Instrumentation_api.Output.formatter cfg.output;
  (module M : Instrumentation_api.Handler.S)
