open Common.Error
open Common.Source
open Common.Attempt

exception ElabError of region * failtrace list

type elaboration_error = region * failtrace list

(* Elaboration errors *)

let error_with_traces (failtraces : failtrace list) =
  raise (ElabError (region_of_failtraces failtraces, failtraces))

let error (at : region) (msg : string) =
  raise (ElabError (at, [ Failtrace (at, msg, []) ]))

let warn (at : region) (msg : string) = warn at "elab" msg

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
