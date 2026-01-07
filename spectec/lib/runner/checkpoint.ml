(* Checkpoint module for coverage run persistence.

   Enables long-running coverage runs to be interrupted and resumed.
   Saves accumulated coverage state and list of completed test inputs.

   Usage:
     (* Configure checkpointing *)
     let config = Checkpoint.{
       output_file = Some "coverage.ckpt";
       resume_from = None;
       save_interval = 100;
     } in

     (* During run *)
     let checkpoint = Checkpoint.create ~spec_files ~completed_inputs:["test1"; "test2"] in
     Checkpoint.save ~file:"coverage.ckpt" checkpoint;

     (* On resume *)
     let checkpoint = Checkpoint.load ~file:"coverage.ckpt" in
     if Checkpoint.verify_spec checkpoint ~spec_files then
       (* filter out completed inputs and continue *)
*)

(* Configuration for checkpointing behavior *)
type config = {
  output_file : string option; (* File to save checkpoints to *)
  resume_from : string option; (* File to resume from, if any *)
  save_interval : int; (* Save checkpoint every N tests *)
}

let default_config =
  { output_file = None; resume_from = None; save_interval = 100 }

(* Coverage state from handlers - extensible for new handlers *)
type coverage_state = {
  branch : Instrumentation.Branch_coverage.result option;
  node_il : Instrumentation.Node_coverage_il.result option;
  node_sl : Instrumentation.Node_coverage_sl.result option;
}

let empty_coverage = { branch = None; node_il = None; node_sl = None }

(* Main checkpoint type - saved/loaded state *)
type t = {
  spec_hash : string; (* MD5 of concatenated spec file contents *)
  completed_inputs : string list; (* IDs of processed test cases *)
  coverage : coverage_state;
  timestamp : float; (* Unix timestamp *)
}

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

(* Create a new checkpoint *)
let create ~spec_files ~completed_inputs ~coverage =
  {
    spec_hash = compute_spec_hash spec_files;
    completed_inputs;
    coverage;
    timestamp = Unix.gettimeofday ();
  }

(* Save checkpoint to file using Marshal *)
let save ~file checkpoint =
  let output_channel = open_out_bin file in
  Marshal.to_channel output_channel checkpoint [];
  close_out output_channel

(* Load checkpoint from file *)
let load ~file =
  let input_channel = open_in_bin file in
  let checkpoint : t = Marshal.from_channel input_channel in
  close_in input_channel;
  checkpoint

(* Verify that spec files haven't changed *)
let verify_spec checkpoint ~spec_files =
  let current_hash = compute_spec_hash spec_files in
  if checkpoint.spec_hash = current_hash then Ok ()
  else
    Error
      (Printf.sprintf
         "Spec files have changed since checkpoint was created.\n\
          Expected hash: %s\n\
          Current hash:  %s"
         checkpoint.spec_hash current_hash)

(* Filter out already-completed inputs *)
let filter_remaining checkpoint inputs ~get_id =
  List.filter
    (fun input -> not (List.mem (get_id input) checkpoint.completed_inputs))
    inputs

(* Human-readable summary of checkpoint *)
let summary checkpoint =
  Printf.sprintf "Checkpoint: %d tests completed, saved at %s"
    (List.length checkpoint.completed_inputs)
    ( Unix.gmtime checkpoint.timestamp |> fun time_record ->
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d UTC"
        (time_record.tm_year + 1900)
        (time_record.tm_mon + 1) time_record.tm_mday time_record.tm_hour
        time_record.tm_min time_record.tm_sec )

(* Display full checkpoint report with coverage data.
   Uses provided config for output destinations, or defaults to Full/stdout. *)
let display_report ~(config : Instrumentation.Config.t) checkpoint =
  Format.printf "=== Checkpoint Contents ===\n\n";
  Format.printf "%s\n\n" (summary checkpoint);
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
  Instrumentation.Dispatcher.set_handlers handlers;
  (* Restore state from checkpoint data *)
  (match checkpoint.coverage.branch with
  | Some branch_result -> Instrumentation.Branch_coverage.restore branch_result
  | None -> ());
  (match checkpoint.coverage.node_il with
  | Some node_result -> Instrumentation.Node_coverage_il.restore node_result
  | None -> ());
  (match checkpoint.coverage.node_sl with
  | Some node_result -> Instrumentation.Node_coverage_sl.restore node_result
  | None -> ());
  (* Call finish to print the reports *)
  Instrumentation.Dispatcher.finish ();
  Instrumentation.Config.close_outputs config
