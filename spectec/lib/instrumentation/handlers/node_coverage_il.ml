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
module Il = Lang.Il
open Instrumentation_core.Util

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
  let il_spec : Il.spec ref = ref []
  let prems_attempted : (region * string, int) Hashtbl.t = Hashtbl.create 256
  let prems_succeeded : (region * string, int) Hashtbl.t = Hashtbl.create 256
  let total_prems = ref 0

  let reset () =
    il_spec := [];
    Hashtbl.clear prems_attempted;
    Hashtbl.clear prems_succeeded;
    total_prems := 0

  let incr tbl key =
    let count = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
    Hashtbl.replace tbl key (count + 1)
end

(* Create a unique key for a premise using region + content prefix *)
let prem_key prem =
  let content = Il.Print.string_of_prem prem |> normalize_whitespace in
  (prem.at, truncate 30 content)

module M : Instrumentation_core.Handler.S = struct
  let rec count_prem prem =
    State.total_prems := !State.total_prems + 1;
    match prem.it with Il.IterPr (inner, _) -> count_prem inner | _ -> ()

  let init ~spec =
    State.reset ();
    match spec with
    | Instrumentation_core.Handler.IlSpec il_spec ->
        State.il_spec := il_spec;
        List.iter
          (fun def ->
            match def.it with
            | Il.RelD (_, _, _, rules) ->
                List.iter
                  (fun rule ->
                    let _, _, prems = rule.it in
                    List.iter count_prem prems)
                  rules
            | Il.DecD (_, _, _, _, clauses) ->
                List.iter
                  (fun clause ->
                    let _, _, prems = clause.it in
                    List.iter count_prem prems)
                  clauses
            | Il.TypD _ -> ())
          il_spec
    | Instrumentation_core.Handler.SlSpec _ -> ()

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
    State.incr State.prems_attempted (prem_key prem)

  let on_prem_exit ~prem ~at:_ ~success =
    if success then State.incr State.prems_succeeded (prem_key prem)

  let on_instr = Instrumentation_core.Noop.on_instr

  (* --- Output: Summary mode (stats + uncovered only) --- *)

  let print_stats () =
    let succeeded = Hashtbl.length State.prems_succeeded in
    let attempted = Hashtbl.length State.prems_attempted in
    let total = !State.total_prems in
    if total > 0 then
      Format.fprintf !fmt
        "IL Premises: %d/%d succeeded (%.2f%%), %d/%d attempted (%.2f%%)\n"
        succeeded total
        (percentage succeeded total)
        attempted total
        (percentage attempted total)

  let print_uncovered () =
    let total = !State.total_prems in
    if total > 0 then (
      (* Collect uncovered - premises never successfully executed *)
      let uncovered = ref [] in
      List.iter
        (fun def ->
          match def.it with
          | Il.RelD (id, _, _, rules) ->
              List.iter
                (fun rule ->
                  let rule_id, _, prems = rule.it in
                  List.iter
                    (fun prem ->
                      if not (Hashtbl.mem State.prems_succeeded (prem_key prem))
                      then
                        uncovered :=
                          (id.it, rule_id.it, Il.Print.string_of_prem prem)
                          :: !uncovered)
                    prems)
                rules
          | Il.DecD (id, _, _, _, clauses) ->
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
                            Il.Print.string_of_prem prem )
                          :: !uncovered)
                    prems)
                clauses
          | Il.TypD _ -> ())
        !State.il_spec;
      if !uncovered <> [] then (
        Format.fprintf !fmt "\nNever succeeded:\n";
        List.iter
          (fun (rel, rule, content) ->
            Format.fprintf !fmt "  %s/%s:\n    %s\n" rel rule
              (normalize_whitespace content))
          (List.rev !uncovered)))

  (* --- Output: Full mode (GCOV-style annotated spec) --- *)

  let fmt_count tbl key =
    match Hashtbl.find_opt tbl key with
    | Some n -> Format.sprintf "%4d" n
    | None -> "####"

  let get_prem_succeeded key =
    Hashtbl.find_opt State.prems_succeeded key |> Option.value ~default:0

  let print_prem indent prem =
    let count = fmt_count State.prems_attempted (prem_key prem) in
    let content = Il.Print.string_of_prem prem |> normalize_whitespace in
    Format.fprintf !fmt "%s  %s-- %s\n" count indent content

  let print_prems indent prems =
    List.iter (print_prem indent) prems;
    (* Print success count for final premise *)
    match List.rev prems with
    | last :: _ ->
        let succ = get_prem_succeeded (prem_key last) in
        let count = if succ > 0 then Format.sprintf "%4d" succ else "####" in
        Format.fprintf !fmt "%s  %sSUCCESS\n" count indent
    | [] -> ()

  let print_full () =
    List.iter
      (fun def ->
        match def.it with
        | Il.RelD (id, _, _, rules) ->
            Format.fprintf !fmt "\nrelation %s:\n" id.it;
            List.iter
              (fun rule ->
                let rule_id, _, prems = rule.it in
                Format.fprintf !fmt "      rule %s:\n" rule_id.it;
                print_prems "    " prems)
              rules
        | Il.DecD (id, _, _, _, clauses) ->
            Format.fprintf !fmt "\ndef $%s:\n" id.it;
            List.iteri
              (fun idx clause ->
                let _, _, prems = clause.it in
                Format.fprintf !fmt "      clause %d:\n" idx;
                print_prems "    " prems)
              clauses
        | Il.TypD _ -> ())
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
  total_prems : int;
}

let get_result () =
  {
    prems_attempted = State.prems_attempted |> Hashtbl.to_seq |> List.of_seq;
    prems_succeeded = State.prems_succeeded |> Hashtbl.to_seq |> List.of_seq;
    total_prems = !State.total_prems;
  }

(* Restore state from a previous result (for checkpoint resume) *)
let restore result =
  Hashtbl.clear State.prems_attempted;
  Hashtbl.clear State.prems_succeeded;
  List.iter
    (fun (key, count) -> Hashtbl.replace State.prems_attempted key count)
    result.prems_attempted;
  List.iter
    (fun (key, count) -> Hashtbl.replace State.prems_succeeded key count)
    result.prems_succeeded;
  State.total_prems := result.total_prems

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
