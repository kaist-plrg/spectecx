open Source

(* Backtracking *)

(* [Guard] marks an applicability-guard failure: the rule did not apply. *)
type failtrace = { region : region; message : string; kind : kind }
and kind = Failed of failtrace list | Guard

type 'a attempt = ('a, failtrace list) result

(* Depth of a failtrace tree *)
let rec depth (failtrace : failtrace) : int =
  match failtrace.kind with
  | Failed subfailtraces ->
      1 + (List.map depth subfailtraces |> List.fold_left max 0)
  | Guard -> 1

(* Constructors *)
let fail (at : region) (msg : string) : 'a attempt =
  Error [ { region = at; message = msg; kind = Failed [] } ]

let fail_guard (at : region) (msg : string) : 'a attempt =
  Error [ { region = at; message = msg; kind = Guard } ]

(* Fail with no messages *)
let fail_silent : 'a attempt = Error []

(* Fail if condition is not met *)
let guard (cond : bool) (at : region) (msg : string) : unit attempt =
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
      Error [ { region = at; message = msg; kind = Failed failtraces } ]

(* Extract region from failtraces *)

let rec region_of_failtrace failtrace =
  if failtrace.region <> no_region then failtrace.region
  else
    match failtrace.kind with
    | Failed failtraces -> region_of_failtraces failtraces
    | Guard -> no_region

and region_of_failtraces failtraces =
  match failtraces with
  | [] -> no_region
  | [ failtrace ] -> region_of_failtrace failtrace
  | failtrace :: _ -> region_of_failtrace failtrace

let compare_failtrace failtrace_l failtrace_r =
  compare_region failtrace_l.region failtrace_r.region

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

(* Flatten error with backtracking failtraces into a single message *)

let rec string_of_failtrace ?(indent = "") ?(level = 0)
    ~(region_parent : region) ~(is_last : bool) ~(depth : int)
    ~(bullet : string) (failtrace : failtrace) : string =
  let { region; message = msg; kind } = failtrace in
  let subfailtraces = match kind with Failed s -> s | Guard -> [] in
  let is_root = level = 0 in
  let smsg =
    if level < depth then ""
    else
      let prefix = if is_root then "" else if is_last then "└── " else "├── " in
      let indent_prefix = String.make 4 ' ' in
      Format.asprintf "%s%s%s%s%s\n" indent prefix
        (if region_parent = region || region = no_region then ""
         else string_of_region region ^ "\n" ^ indent ^ indent_prefix)
        bullet msg
  in
  let region_parent = if region = no_region then region_parent else region in
  let indent =
    if is_root then indent
    else if is_last then indent ^ "    "
    else indent ^ "│   "
  in
  Format.asprintf "%s%s" smsg
    (string_of_failtraces ~indent ~level:(level + 1) ~region_parent ~depth
       subfailtraces)

and string_of_failtraces ?(indent = "") ?(level = 0) ~(region_parent : region)
    ~(depth : int) (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      string_of_failtrace ~indent ~level ~region_parent ~is_last:true ~depth
        ~bullet:"" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          let is_last = idx = List.length failtraces - 1 in
          let bullet = string_of_int (idx + 1) ^ ". " in
          string_of_failtrace ~indent ~level ~region_parent ~is_last ~depth
            ~bullet failtrace)
        failtraces
      |> String.concat ""
