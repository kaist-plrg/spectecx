include Common.Attempt

let error_with_failtraces (failtraces : failtrace list) =
  raise (Error.BacktrackError failtraces)

let unwrap_or_error (attempt : 'a attempt) : 'a =
  match attempt with Ok a -> a | Error traces -> error_with_failtraces traces

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  f (unwrap_or_error attempt)

type ('a, 'label) unique_choice =
  | Unique of 'label * 'a
  | Overlapping of 'label * 'label
  | Unmatched of failtrace list

let choice_unique (alternatives : ('label * (unit -> 'a attempt)) list) :
    ('a, 'label) unique_choice =
  List.fold_left
    (fun outcome (label, attempt) ->
      match outcome with
      | Overlapping _ -> outcome
      | Unique (label_first, _) -> (
          match attempt () with
          | Ok _ -> Overlapping (label_first, label)
          | Error _ -> outcome)
      | Unmatched failtraces -> (
          match attempt () with
          | Ok a -> Unique (label, a)
          | Error failtraces_more -> Unmatched (failtraces @ failtraces_more)))
    (Unmatched []) alternatives

let choice_fallback (failtraces_ordinary : failtrace list)
    (alternatives : (unit -> 'a attempt) list) : 'a attempt =
  match alternatives with
  | [] -> Error failtraces_ordinary
  | _ -> (
      match choice alternatives with
      | Ok a -> Ok a
      | Error failtraces_fallback ->
          Error (failtraces_ordinary @ failtraces_fallback))
