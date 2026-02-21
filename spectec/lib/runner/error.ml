open Common.Source
open Common.Attempt

type t =
  | ParseError of region * string
  | RoundtripError of region * string
  | ElaborateError of Pass.Elaborate.elaboration_error list
  | EvalIlError of region * string
  | EvalSlError of region * string
  | TaskParseError of region * string
  | SpecMismatchError of string * string
  | DirectoryError of string

let string_of_error' at msg =
  if at = no_region then msg else string_of_region at ^ "Error: " ^ msg

let string_of_elab_error at failtraces : string =
  (if at = no_region then "" else string_of_region at ^ "Error:\n")
  ^ string_of_failtraces ~region_parent:at ~depth:0 failtraces

let string_of_elab_errors (errors : Pass.Elaborate.elaboration_error list) :
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
  | RoundtripError (at, msg) -> string_of_error' at msg
  | ElaborateError elab_errs -> string_of_elab_errors elab_errs
  | EvalIlError (at, msg) -> string_of_error' at msg
  | EvalSlError (at, msg) -> string_of_error' at msg
  | TaskParseError (at, msg) -> string_of_error' at msg
  | SpecMismatchError (hash_expected, hash_actual) ->
      Printf.sprintf "Spec version mismatch: expected spec hash %s but got %s."
        hash_expected hash_actual
  | DirectoryError msg -> msg
