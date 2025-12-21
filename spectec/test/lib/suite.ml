(** Test suite runner infrastructure *)

open Core

type expectation = Expect_success | Expect_failure

type config = {
  name : string;
  intro : string;
  heading : string;
  success : string;
  failure : string;
  expected_failure : string;
  unexpected_success : string;
}

let run ~(config : config) ~(exclude_set : Exclude.t) ~(filenames : string list)
    ~(expectation : expectation)
    ~(run : string -> (unit, Runner.Error.t) result) =
  let total = List.length filenames in
  Format.printf "%s %d files\n\n" config.intro total;
  let stats =
    List.fold filenames ~init:Stats.empty ~f:(fun stats filename ->
        Format.printf ">>> Running %s on %s\n" config.heading filename;
        if Exclude.mem exclude_set filename then (
          Format.printf "Excluding file: %s\n\n" filename;
          Stats.add_skip stats ~label:filename)
        else
          let duration, result = Timer.time (fun () -> run filename) in
          let stats =
            match (expectation, result) with
            | Expect_success, Ok () ->
                Format.printf "%s: %s\n\n" config.success filename;
                Stats.add_pass stats ~label:filename ~duration
            | Expect_success, Error err ->
                Format.printf "%s: %s\n  %s\n\n" config.failure filename
                  (Runner.Error.string_of_error err);
                Stats.add_fail stats ~label:filename ~duration
                  (Stats.failure_from_runner err)
            | Expect_failure, Ok () ->
                Format.printf "%s: %s\n\n" config.unexpected_success filename;
                Stats.add_fail stats ~label:filename ~duration
                  Stats.failure_unexpected_success
            | Expect_failure, Error err ->
                Format.printf "%s: %s\n  %s\n\n" config.expected_failure
                  filename
                  (Runner.Error.string_of_error err);
                Stats.add_pass stats ~label:filename ~duration
          in
          stats)
  in
  Stats.print_summary config.name stats;
  Format.printf "\n%!"
