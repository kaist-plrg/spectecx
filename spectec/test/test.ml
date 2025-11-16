open Core
open Util.Source
module Error = Runner.Error

let version = "0.1"

module Filenames = Set

module Timer = struct
  let now () = Core_unix.gettimeofday ()

  let time f =
    let start = now () in
    let result = f () in
    let duration = now () -. start in
    (duration, result)
end

module Files = struct
  let skip_dirs = String.Set.of_list [ "include" ]

  let rec gather acc ~suffix dir =
    let entries = Sys_unix.readdir dir in
    Array.sort entries ~compare:String.compare;
    Array.fold entries ~init:acc ~f:(fun acc entry ->
        let path = Filename.concat dir entry in
        if Sys_unix.is_directory_exn path then
          if Filenames.mem skip_dirs entry then acc else gather acc ~suffix path
        else if String.is_suffix path ~suffix then path :: acc
        else acc)

  let collect ~suffix dir = gather [] ~suffix dir |> List.rev
end

module Exclude = struct
  module Excludes = Set.Make (String)

  let normalize line = "../../../../p4-tests/tests/" ^ line

  let from_file filename =
    In_channel.read_lines filename
    |> List.filter_map ~f:(fun line ->
           let trimmed = String.strip line in
           if String.is_empty trimmed then None
           else if Char.equal trimmed.[0] '#' then None
           else Some (normalize trimmed))

  let load paths =
    let files = List.concat_map paths ~f:(Files.collect ~suffix:".exclude") in
    files |> List.concat_map ~f:from_file |> Excludes.of_list

  let mem set filename = Set.mem set filename
end

module Stats = struct
  type failure = Runner_error of Error.t | Unexpected_success

  type float_summary = {
    count : int;
    sum : float;
    mean : float option;
    median : float option;
    p95 : float option;
    min : float option;
    max : float option;
  }

  type t = {
    passed : int;
    failed : int;
    skipped : int;
    failures : (string * failure) list;
    passed_durations : float list;
    failed_durations : float list;
  }

  let empty : t =
    {
      passed = 0;
      failed = 0;
      skipped = 0;
      failures = [];
      passed_durations = [];
      failed_durations = [];
    }

  let add_pass (stat : t) ~(label : string) ~(duration : float) : t =
    {
      stat with
      passed = stat.passed + 1;
      passed_durations = duration :: stat.passed_durations;
    }

  let add_fail (stat : t) ~(label : string) ~(duration : float)
      (failure : failure) : t =
    {
      stat with
      failed = stat.failed + 1;
      failures = (label, failure) :: stat.failures;
      failed_durations = duration :: stat.failed_durations;
    }

  let add_skip (stat : t) ~(label : string) : t =
    { stat with skipped = stat.skipped + 1 }

  let failure_from_runner err = Runner_error err
  let failure_unexpected_success = Unexpected_success

  let summarize_floats values =
    let count = List.length values in
    let sum = List.fold values ~init:0.0 ~f:( +. ) in
    if Int.equal count 0 then
      {
        count;
        sum;
        mean = None;
        median = None;
        p95 = None;
        min = None;
        max = None;
      }
    else
      let sorted = List.sort values ~compare:Float.compare in
      let mean = Some (sum /. Float.of_int count) in
      let min = List.hd sorted in
      let max = List.last sorted in
      let median =
        if Int.( % ) count 2 = 1 then List.nth sorted (count / 2)
        else
          let lower = List.nth sorted ((count / 2) - 1) in
          let upper = List.nth sorted (count / 2) in
          Option.both lower upper
          |> Option.map ~f:(fun (l, u) -> (l +. u) /. 2.0)
      in
      let percentile p =
        let index = Float.of_int (count - 1) *. p |> Float.iround_down_exn in
        List.nth sorted index
      in
      { count; sum; mean; median; p95 = percentile 0.95; min; max }

  let format_float value = Printf.sprintf "%.6f" value

  let format_float_option = function
    | None -> "n/a"
    | Some value -> format_float value

  let print_summary name (stat : t) =
    let {
      passed;
      failed;
      skipped;
      failures;
      passed_durations;
      failed_durations;
    } =
      stat
    in
    let total = passed + failed + skipped in
    let executed = passed + failed in
    let executed_rate =
      if total = 0 then 0.0
      else float_of_int executed /. float_of_int total *. 100.0
    in
    let skip_rate =
      if total = 0 then 0.0
      else float_of_int skipped /. float_of_int total *. 100.0
    in
    let pass_rate =
      if executed = 0 then 0.0
      else float_of_int passed /. float_of_int executed *. 100.0
    in
    let fail_rate =
      if executed = 0 then 0.0
      else float_of_int failed /. float_of_int executed *. 100.0
    in
    let duration_pass = summarize_floats passed_durations in
    let duration_fail = summarize_floats failed_durations in
    let duration_all =
      summarize_floats (stat.passed_durations @ failed_durations)
    in
    let title = String.capitalize name in
    Format.printf "%s summary\n" title;
    Format.printf "  Ran:     %6d / %-6d (%.2f%%)\n" executed total
      executed_rate;
    Format.printf "  Passed:  %6d (%.2f%% of ran)\n" stat.passed pass_rate;
    Format.printf "  Failed:  %6d (%.2f%% of ran)\n" stat.failed fail_rate;
    Format.printf "  Skipped: %6d (%.2f%% of total)\n\n" stat.skipped skip_rate;
    Format.printf "  Duration (s):\n";

    Format.printf "    Total:   %9.3f\n" duration_all.sum;
    Format.printf "    Mean:    %9s\n" (format_float_option duration_all.mean);
    Format.printf "    Median:  %9s\n" (format_float_option duration_all.median);
    Format.printf "    Min:     %9s\n" (format_float_option duration_all.min);
    Format.printf "    Max:     %9s\n" (format_float_option duration_all.max);
    Format.printf "    P95:     %9s\n" (format_float_option duration_all.p95);

    if stat.passed > 0 && stat.failed > 0 then (
      Format.printf "  Pass Stats (s):\n";
      Format.printf "    Mean:    %9s  Min: %9s  Max: %9s\n"
        (format_float_option duration_pass.mean)
        (format_float_option duration_pass.min)
        (format_float_option duration_pass.max);
      Format.printf "  Fail Stats (s):\n";
      Format.printf "    Mean:    %9s  Min: %9s  Max: %9s\n"
        (format_float_option duration_fail.mean)
        (format_float_option duration_fail.min)
        (format_float_option duration_fail.max));
    if stat.failed > 0 then (
      Format.printf "\nFailures (%d):\n" stat.failed;
      List.iter (List.rev stat.failures) ~f:(fun (label, failure) ->
          match failure with
          | Runner_error err ->
              Format.printf "  [FAIL] %s\n    %s\n" label
                (Error.string_of_error err)
          | Unexpected_success ->
              Format.printf "  [FAIL] %s\n    expected failure but succeeded\n"
                label));

    Format.printf "%!" (* Flush the output *)
end

type expectation = Expect_success | Expect_failure

type suite = {
  name : string;
  intro : string;
  heading : string;
  success : string;
  failure : string;
  expected_failure : string;
  unexpected_success : string;
}

let quiet_parser_logs () =
  Core_unix.putenv ~key:"P4SPEC_LEXER_DEBUG" ~data:"quiet";
  Core_unix.putenv ~key:"P4SPEC_PARSER_DEBUG" ~data:"quiet";
  Core_unix.putenv ~key:"P4SPEC_CONTEXT_DEBUG" ~data:"quiet"

let parser_roundtrip spec_il includes filename =
  let open Core.Result.Let_syntax in
  let%bind program = Runner.parse_p4_file includes filename in
  let unparsed =
    Format.asprintf "%a\n" (Concrete.Pp.pp_program spec_il) program
  in
  let%bind program_rt = Runner.parse_p4_string filename unparsed in
  if Il.Eq.eq_value ~dbg:true program program_rt then Ok ()
  else Error (Error.RoundtripError (no_region, "Roundtrip failed"))

let run_suite ~suite ~exclude_set ~filenames ~expectation ~run =
  let total = List.length filenames in
  Format.printf "%s %d files\n\n" suite.intro total;
  let stats =
    List.fold filenames ~init:Stats.empty ~f:(fun stats filename ->
        Format.printf ">>> Running %s on %s\n" suite.heading filename;
        if Exclude.mem exclude_set filename then (
          Format.printf "Excluding file: %s\n\n" filename;
          Stats.add_skip stats ~label:filename)
        else
          let duration, result = Timer.time (fun () -> run filename) in
          let stats =
            match (expectation, result) with
            | Expect_success, Ok () ->
                Format.printf "%s: %s\n\n" suite.success filename;
                Stats.add_pass stats ~label:filename ~duration
            | Expect_success, Error err ->
                Format.printf "%s: %s\n  %s\n\n" suite.failure filename
                  (Error.string_of_error err);
                Stats.add_fail stats ~label:filename ~duration
                  (Stats.failure_from_runner err)
            | Expect_failure, Ok () ->
                Format.printf "%s: %s\n\n" suite.unexpected_success filename;
                Stats.add_fail stats ~label:filename ~duration
                  Stats.failure_unexpected_success
            | Expect_failure, Error err ->
                Format.printf "%s: %s\n  %s\n\n" suite.expected_failure filename
                  (Error.string_of_error err);
                Stats.add_pass stats ~label:filename ~duration
          in
          stats)
  in
  Stats.print_summary suite.name stats;
  Format.printf "\n%!"

let run_elab specdir =
  let open Core.Result.Let_syntax in
  let spec_il =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Runner.parse_spec_files spec_files in
    let%bind spec_il = Runner.elaborate spec in
    Ok spec_il
  in
  match spec_il with
  | Ok spec_il -> Format.printf "%s\n" (Il.Print.string_of_spec spec_il)
  | Error err ->
      Format.printf "Elaboration failed:\n  %s\n" (Error.string_of_error err)

let run_structure specdir =
  let open Core.Result.Let_syntax in
  let spec_sl =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Runner.parse_spec_files spec_files in
    let%bind spec_il = Runner.elaborate spec in
    let spec_sl = Runner.structure spec_il in
    Ok spec_sl
  in
  match spec_sl with
  | Error err ->
      Format.printf "Structuring failed:\n  %s\n" (Error.string_of_error err)
  | Ok spec_sl -> Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl)

let run_parser includes exclude_dirs testdir specdir =
  quiet_parser_logs ();
  let open Core.Result.Let_syntax in
  let suite_result =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Runner.parse_spec_files spec_files in
    let%bind spec_il = Runner.elaborate spec in
    let filenames = Files.collect ~suffix:".p4" testdir in
    let exclude_set = Exclude.load exclude_dirs in
    let suite =
      {
        name = "parser";
        intro = "Running parser tests on";
        heading = "parser test";
        success = "Parser roundtrip success";
        failure = "Parser roundtrip failed";
        expected_failure = "Expected parser failure";
        unexpected_success = "Unexpected parser success";
      }
    in
    run_suite ~suite ~exclude_set ~filenames ~expectation:Expect_success
      ~run:(fun filename -> parser_roundtrip spec_il includes filename);
    Ok ()
  in
  match suite_result with
  | Ok () -> ()
  | Error err ->
      Format.printf "Failed to elaborate spec:\n  %s\n"
        (Error.string_of_error err)

let run_il ?(negative = false) specdir includes exclude_dirs testdir =
  let open Core.Result.Let_syntax in
  let suite_result =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Runner.parse_spec_files spec_files in
    let%bind spec_il = Runner.elaborate spec in
    let filenames = Files.collect ~suffix:".p4" testdir in
    let exclude_set = Exclude.load exclude_dirs in
    let suite =
      {
        name = "il typing";
        intro = "Running typing test on";
        heading = "typing test";
        success = "Typecheck success";
        failure = "Typecheck failed";
        expected_failure = "Expected typing failure";
        unexpected_success = "Unexpected typing success";
      }
    in
    let expectation = if negative then Expect_failure else Expect_success in
    run_suite ~suite ~exclude_set ~filenames ~expectation ~run:(fun filename ->
        Runner.interp_il ~debug:false ~profile:false spec_il includes filename
        |> Result.map ~f:(fun _ -> ()));
    Ok ()
  in
  match suite_result with
  | Ok () -> ()
  | Error err ->
      Format.printf "Failed to elaborate spec:\n  %s\n"
        (Error.string_of_error err)

let run_sl ?(negative = false) specdir includes exclude_dirs testdir =
  let open Core.Result.Let_syntax in
  let suite_result =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Runner.parse_spec_files spec_files in
    let%bind spec_il = Runner.elaborate spec in
    let spec_sl = Structure.Struct.struct_spec spec_il in
    let filenames = Files.collect ~suffix:".p4" testdir in
    let exclude_set = Exclude.load exclude_dirs in
    let suite =
      {
        name = "sl typing";
        intro = "Running typing test on";
        heading = "typing test";
        success = "Typecheck success";
        failure = "Typecheck failed";
        expected_failure = "Expected typing failure";
        unexpected_success = "Unexpected typing success";
      }
    in
    let expectation = if negative then Expect_failure else Expect_success in
    run_suite ~suite ~exclude_set ~filenames ~expectation ~run:(fun filename ->
        Runner.interp_sl spec_sl includes filename
        |> Result.map ~f:(fun _ -> ()));
    Ok ()
  in
  match suite_result with
  | Ok () -> ()
  | Error err ->
      Format.printf "Failed to elaborate spec:\n  %s\n"
        (Error.string_of_error err)

let elab_command =
  Core.Command.basic ~summary:"run elaboration test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () -> run_elab specdir)

let structure_command =
  Core.Command.basic ~summary:"run structuring test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () -> run_structure specdir)

let run_il_command =
  Core.Command.basic ~summary:"run typing test on IL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes = flag "-i" (listed string) ~doc:"p4 include paths"
     and exclude_dirs = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdir = flag "-d" (required string) ~doc:"p4 test directory"
     and negative =
       flag "-neg" no_arg ~doc:"expect typing failures (negative mode)"
     in
     fun () -> run_il ~negative specdir includes exclude_dirs testdir)

let run_sl_command =
  Core.Command.basic ~summary:"run typing test on SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes = flag "-i" (listed string) ~doc:"p4 include paths"
     and exclude_dirs = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdir = flag "-d" (required string) ~doc:"p4 test directory"
     and negative =
       flag "-neg" no_arg ~doc:"expect typing failures (negative mode)"
     in
     fun () -> run_sl ~negative specdir includes exclude_dirs testdir)

let run_parser_command =
  Core.Command.basic ~summary:"run parser test on P4 files"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"p4 include paths"
     and exclude_dirs = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdir = flag "-d" (required string) ~doc:"p4 test directory"
     and specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () -> run_parser includes exclude_dirs testdir specdir)

let command =
  Core.Command.group ~summary:"p4spec-test"
    [
      ("elab", elab_command);
      ("run-il", run_il_command);
      ("run-sl", run_sl_command);
      ("struct", structure_command);
      ("parser", run_parser_command);
    ]

let () = Command_unix.run ~version command
