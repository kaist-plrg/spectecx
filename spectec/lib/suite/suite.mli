(** Suite - Test suite infrastructure, batch running, and checkpoint
    persistence. *)

(** {1 Checkpoint} *)

module Checkpoint : sig
  type config = {
    output_file : string option;
    resume_from : string option;
    save_interval : int;
  }

  val default_config : config

  type coverage = (string * bytes) list

  type t = {
    version : int;
    spec_hash : string;
    completed_inputs : string list;
    coverage : coverage;
    timestamp : float;
  }

  val load_from_file : file:string -> (t, Spectec.Error.t) result
  val save_to_file : file:string -> t -> unit

  val verify_and_load :
    file:string ->
    spec_files:string list ->
    verbose:bool ->
    (t, Spectec.Error.t) result

  val filter_remaining : t -> 'a list -> get_id:('a -> string) -> 'a list
  val restore_coverage : t -> unit

  val save :
    spec_files:string list ->
    completed_inputs:string list ->
    output_file:string option ->
    unit

  val display_report :
    spec:Lang.Il.spec -> config:Instrumentation.Config.t -> t -> unit

  val merge : t -> t -> (t, Spectec.Error.t) result
end

(** {1 Outcome-based runners} *)

type 'i test_result = {
  input : 'i;
  source : string;
  outcome : Spectec.Task.test_outcome;
}

(** Run a single input and compute outcome. Includes full instrumentation
    lifecycle. *)
val run_with_outcome_with_instrumentation :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  Spectec.Task.test_outcome

(** Run a suite of inputs and return individual outcomes. Instrumentation
    lifecycle wraps the entire suite. *)
val run_suite_with_outcomes :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  ?verbose:bool ->
  'i list ->
  'i test_result list

(** {1 Suite summary} *)

type suite_summary = {
  pass : int;
  expected_fail : int;
  fail : int;
  unexpected_pass : int;
  total : int;
}

val summarize_outcomes : 'i test_result list -> suite_summary
val summary_passed : suite_summary -> int
val summary_failed : suite_summary -> int

(** {1 Presentation} *)

val print_outcome :
  (module Spectec.Task.S with type input = 'i) ->
  string ->
  Spectec.Task.test_outcome ->
  unit

val print_summary : suite_summary -> unit

(** {1 Composed run + print} *)

val run_and_print_single :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  unit

val run_and_print_suite :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  verbose:bool ->
  'i list ->
  unit

(** {1 Target batch} *)

type task_result = { task_name : string; summary : suite_summary }

val run_target_batch :
  ?config:Instrumentation.Config.t ->
  ?test_dir:string ->
  checkpoint_config:Checkpoint.config ->
  verbose:bool ->
  sl_mode:bool ->
  spec_files:string list ->
  Lang.Il.spec ->
  Spectec.Task.packed_task list ->
  task_result list
