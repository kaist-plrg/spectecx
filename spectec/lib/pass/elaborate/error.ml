open Common.Source
open Common.Attempt

type single_error = region * failtrace list
type error = single_error list
type 'a result = ('a, error) Stdlib.result

exception ElabError of single_error

(* Elaboration errors *)

let error_with_traces (failtraces : failtrace list) =
  raise (ElabError (region_of_failtraces failtraces, failtraces))

let error (at : region) (msg : string) =
  raise (ElabError (at, [ Failtrace (at, msg, []) ]))

let warn (at : region) (msg : string) = Diag.warn at "elab" msg

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

(* Formatting *)

let single_error_to_string ((at, failtraces) : single_error) : string =
  (if at = no_region then "" else string_of_region at ^ "Error:\n")
  ^ string_of_failtraces ~region_parent:at ~depth:0 failtraces

let to_string (errors : error) : string =
  let sorted = List.sort (fun (l, _) (r, _) -> compare_region l r) errors in
  String.concat "\n" (List.map single_error_to_string sorted)

(* Diagnostic conversion *)

let single_to_diagnostic ((at, failtraces) : single_error) : Diag.t =
  let message, trace =
    match failtraces with
    | [] -> ("elaboration failed", [])
    | [ Failtrace (_, msg, children) ] ->
        (msg, Diag.traces_of_failtraces children)
    | _ -> ("elaboration failed", Diag.traces_of_failtraces failtraces)
  in
  Diag.error ~source:"elab" ~trace at message

let to_diagnostics (errors : error) : Diag.Bag.t =
  List.fold_left
    (fun bag se -> Diag.Bag.add bag (single_to_diagnostic se))
    Diag.Bag.empty errors
