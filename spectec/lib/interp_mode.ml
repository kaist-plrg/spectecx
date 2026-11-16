(** Which interpreter runs a task. Only PL consults hints, so only PL carries a
    hint environment. *)

type t = Al | Sl | Pl of Hints.Henv.t

(** What a caller asked for, before the spec PL needs has been loaded. *)
type request = [ `AL | `SL | `PL ]

let resolve ~henv : request -> t = function
  | `AL -> Al
  | `SL -> Sl
  | `PL -> Pl henv

let to_string : request -> string = function
  | `AL -> "AL"
  | `SL -> "SL"
  | `PL -> "PL"
