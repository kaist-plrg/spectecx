module Il = Lang.Il
module Al = Lang.Al

type 'a result = ('a, Error.t) Stdlib.result

(** Test expectation: does the test expect success or failure? *)
type expectation = Positive | Negative

(** Test outcome after considering expectation *)
type test_outcome =
  | Pass of Il.Value.t list
  | Fail of Error.t
  | ExpectedFail of Error.t
  | UnexpectedPass of Il.Value.t list

(** Compute test outcome from expectation and result *)
let compute_outcome expectation result =
  match (expectation, result) with
  | Positive, Ok values -> Pass values
  | Positive, Error e -> Fail e
  | Negative, Error e -> ExpectedFail e
  | Negative, Ok values -> UnexpectedPass values

(** A task specification for interpreter execution *)
module type S = sig
  val name : string

  (* Reference to the parent Target. *)
  module Target : Target.S

  type input

  (** Parse a string into IL values. *)
  val parse_string : filename:string -> string -> Il.Value.t list result

  (** Unparse IL values back into a string for display. *)
  val unparse : spec:Il.spec -> Il.Value.t list -> string

  (** May report decoded values through {!Instrumentation.Value_event.Decoded}.
  *)
  val parse_input : input -> Il.Value.t list result

  (** [values] must come from [parse_input] on the same input. Calls through
      [eval_rel] and [eval_func] share one context, cache and target state. Both
      callbacks are valid only while [run] runs. [eval_func] follows
      {!Interp.eval_func}. *)
  val run :
    input ->
    values:Il.Value.t list ->
    eval_rel:(string -> Il.Value.t list -> Il.Value.t list result) ->
    eval_func:(string -> Il.Value.t list -> Il.Value.t result) ->
    Il.Value.t list result

  (** Get source identifier for tracing/errors (e.g., filename, test name) *)
  val source : input -> string

  (** Get test expectation (Positive/Negative) *)
  val expectation : input -> expectation

  (** Collect inputs, from [dir] when provided. *)
  val collect : ?dir:string -> unit -> input list

  val format_output : Il.Value.t list -> string
  val save_output : string -> Il.Value.t list -> unit
end

(** Existential wrapper for heterogeneous tasks *)
type packed_task = Pack : (module S with type input = 'a) -> packed_task
