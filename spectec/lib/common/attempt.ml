(* Backtracking *)

(* [Guard] marks an applicability-guard failure: the rule did not apply. *)
type failtrace = { location : Location.t; message : string; kind : kind }
and kind = Failed of failtrace list | Guard

type 'a attempt = ('a, failtrace list) result

(* Depth of a failtrace tree *)
let rec depth (failtrace : failtrace) : int =
  match failtrace.kind with
  | Failed subfailtraces ->
      1 + (List.map depth subfailtraces |> List.fold_left max 0)
  | Guard -> 1

(* Constructors *)
let fail (at : Location.t) (msg : string) : 'a attempt =
  Error [ { location = at; message = msg; kind = Failed [] } ]

let fail_guard (at : Location.t) (msg : string) : 'a attempt =
  Error [ { location = at; message = msg; kind = Guard } ]

(* Fail with no messages *)
let fail_silent : 'a attempt = Error []

(* Fail if condition is not met *)
let guard (cond : bool) (at : Location.t) (msg : string) : unit attempt =
  if cond then Ok () else fail at msg

(* Monadic bind *)
let ( let* ) = Result.bind

(* Combinators *)

(* Try alternatives in order, collecting all failtraces *)
let rec choice = function
  | [] -> fail_silent
  | f :: fs -> (
      match f () with
      | Ok a -> Ok a
      | Error failtraces_h -> (
          match choice fs with
          | Ok a -> Ok a
          | Error failtraces_t -> Error (failtraces_h @ failtraces_t)))

(* Nest failtraces within a new failure message *)
let nest at msg attempt =
  match attempt with
  | Ok a -> Ok a
  | Error failtraces ->
      Error [ { location = at; message = msg; kind = Failed failtraces } ]

(* Extract location from failtraces *)

let rec location_of_failtrace failtrace =
  match (failtrace.location, failtrace.kind) with
  | Location.Unknown, Failed failtraces -> location_of_failtraces failtraces
  | location, _ -> location

and location_of_failtraces failtraces =
  match failtraces with
  | [] -> Location.Unknown
  | [ failtrace ] -> location_of_failtrace failtrace
  | failtrace :: _ -> location_of_failtrace failtrace

let compare_failtrace failtrace_l failtrace_r =
  Location.compare failtrace_l.location failtrace_r.location

(* A rule attempt that failed only its applicability guard did not apply. *)
let is_guard_failure (failtrace : failtrace) : bool =
  match failtrace.kind with
  | Guard -> true
  | Failed [ { kind = Guard; _ } ] -> true
  | _ -> false

let rec prune_failtraces (failtraces : failtrace list) : failtrace list =
  failtraces
  |> List.filter (fun failtrace -> not (is_guard_failure failtrace))
  |> List.map prune_failtrace

and prune_failtrace (failtrace : failtrace) : failtrace =
  match failtrace.kind with
  | Guard -> failtrace
  | Failed subfailtraces ->
      { failtrace with kind = Failed (prune_failtraces subfailtraces) }
