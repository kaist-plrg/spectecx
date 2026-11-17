(** The breakpoints of a session, numbered in the order they are set.

    A breakpoint names a relation ([Rel]), one of its rules ([Rule]), or a
    written premise ([Prem]). Whether a relation or rule exists is the engine's
    to check; this table only keeps them. *)

module Location = Common.Location

type target = Rel of string | Rule of string * string | Prem of Sites.site
type breakpoint = { number : int; target : target }

(** [Rel], [Rel/rule], or [file:line in owner]. *)
val string_of_target : target -> string

type t

val create : unit -> t

(** Adds a breakpoint at [target], or returns the one already there. *)
val add : t -> target -> [ `New of breakpoint | `Existing of breakpoint ]

(** Removes breakpoint [number]; [false] when there is none. *)
val delete : t -> int -> bool

(** In the order they were set. *)
val to_list : t -> breakpoint list

(** The breakpoint at exactly [target]. *)
val find : t -> target -> breakpoint option

(** The breakpoint on the written premise entered at [at]. *)
val on_premise : t -> Location.t -> breakpoint option
