open Util.Source
open Util.Attempt

type t =
  | ParseError of region * string
  | ElabError of Elaborate.Error.elaboration_error list
  | RoundtripError of region * string
  | IlInterpError of region * string
  | SlInterpError of region * string
  | P4ParseError of region * string

let string_of_error' at msg =
  if at = no_region then msg else string_of_region at ^ "Error: " ^ msg

let string_of_elab_error at failtraces : string =
  (if at = no_region then "" else string_of_region at ^ "Error:\n")
  ^ string_of_failtraces ~region_parent:at ~depth:0 failtraces

let string_of_elab_errors (errors : Elaborate.Error.elaboration_error list) :
    string =
  let errors_sorted =
    List.sort (fun (at_l, _) (at_r, _) -> compare_region at_l at_r) errors
  in
  let formatted_errors =
    List.map
      (fun (at, failtraces) -> string_of_elab_error at failtraces)
      errors_sorted
  in
  String.concat "\n" formatted_errors

let string_of_error = function
  | ParseError (at, msg) -> string_of_error' at msg
  | ElabError elab_errs -> string_of_elab_errors elab_errs
  | RoundtripError (at, msg) -> string_of_error' at msg
  | IlInterpError (at, msg) -> string_of_error' at msg
  | SlInterpError (at, msg) -> string_of_error' at msg
  | P4ParseError (at, msg) -> string_of_error' at msg
