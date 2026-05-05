(** QuickCheck test runner.

    [check] runs a property and returns the result ([outcome]).
    [quickcheck] prints the result and raises an exception on failure. *)

(** {2 Configuration} *)

type config = {
  num_tests : int;
  (** Number of test cases to run. Default: 100. *)
  max_size : int;
  (** Maximum size parameter. Grows from 0 to max_size incrementally. Default: 20. *)
  seed : [ `Deterministic of int | `Nondeterministic ];
  (** PRNG seed. Default: [`Deterministic 43] (reproducible). *)
  verbose : bool;
  (** If true, prints each test case. Default: false. *)
}

val default_config : config

(** {2 Outcome type} *)

type outcome =
  | Pass of { num_tests : int; stamps : (string * int) list }
  (** All tests passed. [stamps] holds label frequencies. *)
  | Fail of { num_tests : int; counterexample : string list }
  (** Counterexample found. [counterexample] is the [Result.arguments] field. *)
  | Gave_up of { num_tests : int }
  (** Too many neutral results; gave up. *)

(** {2 Runner} *)

val check : ?config:config -> Property.t -> outcome
(** [check prop] runs [prop] and returns [outcome]. *)

type opt = PROP | GEN

val quickcheck : ?config:config -> Property.t -> opt -> unit
(** [quickcheck prop opt] runs [check] and prints a human-readable report. *)