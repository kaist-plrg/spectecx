(** Which interpreter runs a task. Only PL consults hints, so only PL carries a
    hint environment. *)

type t = Al | Sl | Pl of Hints.Henv.t

(** Mode selection before the caller has the hint environment required by PL. *)
type request = [ `AL | `SL | `PL ]

let resolve ~henv : request -> t = function
  | `AL -> Al
  | `SL -> Sl
  | `PL -> Pl henv

let to_request : t -> request = function Al -> `AL | Sl -> `SL | Pl _ -> `PL

let to_string : request -> string = function
  | `AL -> "AL"
  | `SL -> "SL"
  | `PL -> "PL"
