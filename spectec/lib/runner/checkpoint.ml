(* ============================================================================
   TYPES AND CONFIGURATION
   ============================================================================ *)

(* Configuration for checkpointing behavior *)
type config = {
  output_file : string option; (* File to save checkpoints to *)
  resume_from : string option; (* File to resume from, if any *)
  save_interval : int; (* Save checkpoint every N tests *)
}

let default_config =
  { output_file = None; resume_from = None; save_interval = 100 }

(* Coverage state from handlers - extensible for new handlers *)
type coverage = {
  branch : Instrumentation.Branch_coverage.result option;
  node_il : Instrumentation.Node_coverage_il.result option;
  node_sl : Instrumentation.Node_coverage_sl.result option;
}

(* Main checkpoint type - saved/loaded state *)
type t = {
  spec_hash : string; (* MD5 of concatenated spec file contents *)
  completed_inputs : string list; (* IDs of processed test cases *)
  coverage : coverage;
  timestamp : float; (* Unix timestamp *)
}

(* ============================================================================
   INTERNAL HELPERS
   ============================================================================ *)

(* Compute MD5 hash of spec files for change detection *)
let compute_spec_hash spec_files =
  let contents =
    List.sort String.compare spec_files
    |> List.map (fun filename ->
           let input_channel = open_in filename in
           let length = in_channel_length input_channel in
           let content = really_input_string input_channel length in
           close_in input_channel;
           content)
    |> String.concat "\n"
  in
  Digest.string contents |> Digest.to_hex

(* Create a new checkpoint with current timestamp *)
let create ~spec_files ~completed_inputs ~coverage =
  {
    spec_hash = compute_spec_hash spec_files;
    completed_inputs;
    coverage;
    timestamp = Unix.gettimeofday ();
  }

(* Format checkpoint as human-readable summary string *)
let format_summary checkpoint =
  Printf.sprintf "Checkpoint: %d tests completed, saved at %s"
    (List.length checkpoint.completed_inputs)
    ( Unix.gmtime checkpoint.timestamp |> fun time_record ->
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d UTC"
        (time_record.tm_year + 1900)
        (time_record.tm_mon + 1) time_record.tm_mday time_record.tm_hour
        time_record.tm_min time_record.tm_sec )

(* ============================================================================
   FILE I/O (SERIALIZATION)
   ============================================================================ *)

(* Load checkpoint from file using Marshal.
   Returns Ok checkpoint if successful, Error if file cannot be loaded. *)
let load_from_file ~file =
  try
    let input_channel = open_in_bin file in
    let checkpoint : t = Marshal.from_channel input_channel in
    close_in input_channel;
    Ok checkpoint
  with
  | Sys_error msg ->
      Error
        (Error.DirectoryError
           (Printf.sprintf "Failed to load checkpoint file '%s': %s" file msg))
  | e ->
      Error
        (Error.DirectoryError
           (Printf.sprintf "Failed to load checkpoint file '%s': %s" file
              (Printexc.to_string e)))

(* Save checkpoint to file using Marshal *)
let save_to_file ~file checkpoint =
  let output_channel = open_out_bin file in
  Marshal.to_channel output_channel checkpoint [];
  close_out output_channel

(* ============================================================================
   VALIDATION
   ============================================================================ *)

(* Verify that spec files haven't changed since checkpoint was created *)
let verify_spec checkpoint ~spec_files =
  let current_hash = compute_spec_hash spec_files in
  if checkpoint.spec_hash = current_hash then Ok ()
  else Error (Error.SpecMismatchError (checkpoint.spec_hash, current_hash))

(* ============================================================================
   MERGE OPERATIONS
   ============================================================================ *)

(* Merge two IL node coverage results *)
let merge_node_il_coverage (result1 : Instrumentation.Node_coverage_il.result)
    (result2 : Instrumentation.Node_coverage_il.result) :
    Instrumentation.Node_coverage_il.result =
  (* Helper to merge count lists by summing counts for each key *)
  let merge_counts counts1 counts2 =
    let tbl = Hashtbl.create 256 in
    let add_count key count =
      let existing = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
      Hashtbl.replace tbl key (existing + count)
    in
    List.iter (fun (key, count) -> add_count key count) counts1;
    List.iter (fun (key, count) -> add_count key count) counts2;
    Hashtbl.to_seq tbl |> List.of_seq
  in
  (* Helper to merge test case lists by union (removing duplicates) *)
  let merge_test_lists tests1 tests2 =
    let tbl = Hashtbl.create 256 in
    (* Union two test ID lists, removing duplicates *)
    let union_test_ids existing new_ids =
      List.fold_left
        (fun existing test_id ->
          if List.mem test_id existing then existing else test_id :: existing)
        existing new_ids
    in
    let add_tests key test_ids =
      let existing = Hashtbl.find_opt tbl key |> Option.value ~default:[] in
      Hashtbl.replace tbl key (union_test_ids existing test_ids)
    in
    List.iter (fun (key, test_ids) -> add_tests key test_ids) tests1;
    List.iter (fun (key, test_ids) -> add_tests key test_ids) tests2;
    Hashtbl.to_seq tbl |> List.of_seq
  in
  {
    (* Use from first - should be same if spec matches *)
    Instrumentation.Node_coverage_il.prem_to_uid = result1.prem_to_uid;
    Instrumentation.Node_coverage_il.uid_to_prem = result1.uid_to_prem;
    Instrumentation.Node_coverage_il.total_prems = result1.total_prems;
    Instrumentation.Node_coverage_il.prems_attempted =
      merge_counts result1.prems_attempted result2.prems_attempted;
    Instrumentation.Node_coverage_il.prems_succeeded =
      merge_counts result1.prems_succeeded result2.prems_succeeded;
    Instrumentation.Node_coverage_il.prem_to_test =
      merge_test_lists result1.prem_to_test result2.prem_to_test;
  }

(* Merge two coverage structures.
   For now, only merges IL node coverage. Other coverage types are TODO. *)
let merge_coverage coverage1 coverage2 =
  let node_il =
    match (coverage1.node_il, coverage2.node_il) with
    | Some r1, Some r2 -> Some (merge_node_il_coverage r1 r2)
    | Some r, None | None, Some r -> Some r
    | None, None -> None
  in
  {
    branch = coverage1.branch;
    (* TODO: merge branch coverage *)
    node_il;
    node_sl = coverage1.node_sl;
    (* TODO: merge SL node coverage *)
  }

(* Merge two checkpoints into a new checkpoint.
   - Merges completed_inputs (union)
   - Merges coverage data (IL node coverage merged, others TODO)
   - Uses spec_hash from first checkpoint (they should match)
   - Creates new timestamp *)
let merge checkpoint1 checkpoint2 =
  (* Verify spec hashes match *)
  if checkpoint1.spec_hash <> checkpoint2.spec_hash then
    Error
      (Error.SpecMismatchError (checkpoint1.spec_hash, checkpoint2.spec_hash))
  else
    (* Merge completed inputs (union of both lists, removing duplicates) *)
    let completed_inputs =
      (* Use string -> unit hashtable to track seen IDs *)
      let seen = Hashtbl.create 256 in
      (* Add all IDs from first checkpoint *)
      List.iter
        (fun id -> Hashtbl.replace seen id ())
        checkpoint1.completed_inputs;
      (* Add all IDs from second checkpoint (duplicates automatically handled) *)
      List.iter
        (fun id -> Hashtbl.replace seen id ())
        checkpoint2.completed_inputs;
      (* Collect all unique IDs into a list *)
      Hashtbl.fold (fun id () seen_list -> id :: seen_list) seen []
    in
    (* Merge coverage *)
    let coverage = merge_coverage checkpoint1.coverage checkpoint2.coverage in
    Ok
      {
        spec_hash = checkpoint1.spec_hash;
        completed_inputs;
        coverage;
        timestamp = Unix.gettimeofday ();
      }

(* ============================================================================
   COVERAGE OPERATIONS
   ============================================================================ *)

(* Capture current coverage state from all instrumentation handlers *)
let snapshot_coverage () =
  {
    branch = Some (Instrumentation.Branch_coverage.get_result ());
    node_il = Some (Instrumentation.Node_coverage_il.get_result ());
    node_sl = Some (Instrumentation.Node_coverage_sl.get_result ());
  }

(* Restore coverage state from checkpoint into instrumentation handlers.
   This should be called after instrumentation handlers are initialized
   but before running any tests. *)
let restore_coverage checkpoint =
  (match checkpoint.coverage.branch with
  | Some branch_result -> Instrumentation.Branch_coverage.restore branch_result
  | None -> ());
  (match checkpoint.coverage.node_il with
  | Some node_result -> Instrumentation.Node_coverage_il.restore node_result
  | None -> ());
  match checkpoint.coverage.node_sl with
  | Some node_result -> Instrumentation.Node_coverage_sl.restore node_result
  | None -> ()

(* ============================================================================
   PUBLIC API
   ============================================================================ *)

(* Load and verify checkpoint from file.
   Returns Ok checkpoint if valid, Error if file cannot be loaded or spec mismatch. *)
let verify_and_load ~file ~spec_files ~verbose =
  let ( let* ) = Result.bind in
  let* checkpoint = load_from_file ~file in
  match verify_spec checkpoint ~spec_files with
  | Ok () ->
      if verbose then
        Format.printf "Resuming from checkpoint: %s\n"
          (format_summary checkpoint);
      Ok checkpoint
  | Error e -> Error e

(* Filter out already-completed inputs from a list *)
let filter_remaining checkpoint inputs ~get_id =
  List.filter
    (fun input -> not (List.mem (get_id input) checkpoint.completed_inputs))
    inputs

(* Capture current state and save checkpoint to file.
   Collects current coverage state and completed inputs, then saves to file if configured. *)
let save ~spec_files ~completed_inputs ~output_file =
  match output_file with
  | Some file ->
      let checkpoint =
        create ~spec_files ~completed_inputs ~coverage:(snapshot_coverage ())
      in
      save_to_file ~file checkpoint
  | None -> ()

(* ============================================================================
   DISPLAY
   ============================================================================ *)

(* Display full checkpoint report with coverage data.
   Uses provided config for output destinations, or defaults to Full/stdout. *)
let display_report ~spec ~(config : Instrumentation.Config.t) checkpoint =
  Format.printf "=== Checkpoint Contents ===\n\n";
  Format.printf "%s\n\n" (format_summary checkpoint);
  Format.printf "Completed tests: %d\n\n"
    (List.length checkpoint.completed_inputs);
  (* Use provided config if present, else default to Full/stdout *)
  let branch_cfg =
    match config.branch_coverage with
    | Some cfg -> cfg
    | None ->
        Instrumentation.Branch_coverage.
          { level = Full; output = Instrumentation.Output.stdout }
  in
  let node_il_cfg =
    match config.node_coverage with
    | Some cfg -> cfg
    | None ->
        Instrumentation.Node_coverage_il.
          { level = Full; output = Instrumentation.Output.stdout }
  in
  (* Create handlers with configured outputs *)
  let handlers =
    [
      Instrumentation.Branch_coverage.make branch_cfg;
      Instrumentation.Node_coverage_il.make node_il_cfg;
      Instrumentation.Node_coverage_sl.make node_il_cfg;
    ]
  in
  (* Register static dependencies from all handlers *)
  List.iter
    (fun (module H : Instrumentation.Handler.S) ->
      List.iter
        (fun (module M : Instrumentation_static.Static.S) ->
          Instrumentation_static.Static.register (module M))
        H.static_dependencies)
    handlers;
  (* Initialize Static analysis *)
  Instrumentation_static.Static.reset_all ();
  Instrumentation_static.Static.init_all
    (Instrumentation_static.Static.IlSpec spec);
  Instrumentation.Dispatcher.set_handlers handlers;
  Instrumentation.Dispatcher.init ~spec:(Instrumentation.Handler.IlSpec spec);
  (* Restore state from checkpoint data *)
  restore_coverage checkpoint;
  (* Call finish to print the reports *)
  Instrumentation.Dispatcher.finish ();
  Instrumentation.Config.close_outputs config
