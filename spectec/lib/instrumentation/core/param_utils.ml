(* Lightweight helpers for use inside Spec submodule implementations. *)

(* Look up a param value in the alist — e.g., Param_utils.get alist "level" *)
let get params key = List.assoc_opt key params |> Option.join

(* Convenience: convert optional path string to Output.t *)
let output_of = function None -> Output.stdout | Some p -> Output.file p

(* Generic two-level parser — pass the handler's own Summary/Full constructors.
   E.g., parse_level ~summary:Summary ~full:Full "summary" *)
let parse_level ~summary ~full = function
  | "summary" -> summary
  | "full" -> full
  | s -> failwith ("Invalid level: " ^ s ^ " (expected: summary|full)")

(* Common parameter declarations — handlers include whichever apply *)
let level_param = ("level", "LEVEL verbosity level (e.g., summary|full)")
let output_param = ("output", "FILE output destination file")
