(** Task - Defines a kind of interpreter task for a target.

    A TASK encapsulates how to handle one type of interpreter execution:
    - name: Identifier (e.g., "typecheck", "state_transition")
    - input: Target-specific input type
    - parse: Convert input into relation name + IL values
    - source: Get source identifier for tracing/errors
    - expectation: Whether input expects success or failure
    - collect: Gather inputs from a directory
    - format_output: Format output values for display
    - save_output: Save output to file (optional) *)

module Il = Lang.Il

type 'a pipeline_result = ('a, Error.t) result

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
module type TASK = sig
  val name : string

  type input

  val parse :
    spec:Il.spec -> input -> (string * Il.Value.t list) pipeline_result

  val source : input -> string
  val expectation : input -> expectation
  val collect : string -> input list
  val format_output : Il.Value.t list -> string
  val save_output : string -> Il.Value.t list -> unit
end

(** Existential wrapper for heterogeneous tasks *)
type packed_task = Pack : (module TASK with type input = 'a) -> packed_task
