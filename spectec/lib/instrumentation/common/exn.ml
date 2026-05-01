type captured = exn * Printexc.raw_backtrace

let capture exn : captured = (exn, Printexc.get_raw_backtrace ())
let raise_captured (exn, bt) = Printexc.raise_with_backtrace exn bt

let with_cleanup ~cleanup f =
  try
    let result = f () in
    cleanup ();
    result
  with exn ->
    let captured = capture exn in
    (try cleanup () with _ -> ());
    raise_captured captured

let try_record_first_error first_error f =
  match first_error with
  | Some _ as first_error -> (
      try
        f ();
        first_error
      with _ -> first_error)
  | None -> (
      try
        f ();
        None
      with exn -> Some (capture exn))

let raise_recorded_error = function
  | None -> ()
  | Some captured -> raise_captured captured
