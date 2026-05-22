(** Properties for property-based testing.

    A property is a generator of verdicts: each trial draws one verdict, which
    the runner inspects to decide whether the property passed, failed, or should
    discard the trial. *)

(** {2 Verdicts} *)

module Verdict : sig
  type status = [ `Pass | `Fail | `Discard ]

  type t = {
    status : status;
    arguments : string list;
        (** Counterexample arguments recorded by [for_all]. *)
    stamp : string list;
        (** Labels accumulated by [label]. Used for statistics. *)
    shrink : unit -> t Gen.t list;
        (** Lazy thunk: candidate generators to try for shrinking a failing
            verdict. Populated by [for_all ~shrink] and consumed by [Test.run].
        *)
    generalize : unit -> (string * t Gen.t list) list;
        (** Lazy thunk: generalization candidates as [(label, samples)] pairs. A
            candidate is accepted when every sample still gives [`Fail].
            Populated by [for_all ~generalize] and consumed by [Test.run] after
            shrinking. *)
  }

  val pass : t
  val fail : t
  val discard : t
end

(** {2 Properties} *)

type t = Verdict.t Gen.t

(** Lifts a fixed verdict into a property whose every trial returns that
    verdict. *)
val of_verdict : Verdict.t -> t

(** [for_all ?shrink ?generalize ~show gen body] generates a value with [gen],
    passes it to [body], and records the [show]n value in the verdict's
    arguments. If [shrink] is provided, [Test.run] uses it to find a minimal
    counterexample. If [generalize] is provided, [Test.run] tries each
    [(label, gen')] candidate after shrinking. *)
val for_all :
  ?shrink:('a -> 'a list) ->
  ?generalize:('a -> (string * 'a Gen.t) list) ->
  show:('a -> string) ->
  'a Gen.t ->
  ('a -> t) ->
  t

(** [label s prop] adds [s] to the stamp of every verdict the property produces.
*)
val label : string -> t -> t
