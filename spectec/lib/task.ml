module Il = Lang.Il

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

type run =
  | Relation of string * Il.Value.t list
  | Driver of
      (eval_rel:(string -> Il.Value.t list -> Il.Value.t list result) ->
      Il.Value.t list result)
      (** Calls through [eval_rel] share one context and target state. Use
          [eval_rel] only while the driver runs. *)

(** A task specification for interpreter execution *)
module type S = sig
  val name : string

  (* Reference to the parent Target. *)
  module Target : Target.S

  type input

  (** Parse a string into IL values. *)
  val parse_string :
    spec:Il.spec -> filename:string -> string -> Il.Value.t list result

  (** Unparse IL values back into a string for display. *)
  val unparse : spec:Il.spec -> Il.Value.t list -> string

  (** May report decoded values through {!Instrumentation.Value_event.Decoded}.
  *)
  val parse_input : spec:Il.spec -> input -> run result

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
