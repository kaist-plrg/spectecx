(** Debugger engine: follows an IL run through its events and decides where it
    stops, leaving commands and display to a frontend.

    Breakpoints name a relation ([Rel]), one of its rules ([Rule]), or a written
    premise of a rule or function clause ([Prem]). Stepping stops at the
    premises written in the spec, skipping those elaboration synthesized.

    The engine keeps one debugging session at a time: {!Make} builds the handler
    that runs it, and the other functions act on it, from the frontend's hooks.
*)

module Il = Lang.Il
module Location = Common.Location
module VEnv = Envs_interp.VEnv

(** A request the engine refuses, with a message for the user. *)
exception Error of string

(** {1 Breakpoints} *)

(** A written premise of a rule or clause. [span] is the premise as written;
    [hit] is where it is entered, which is inside [span] for an iterated
    premise. [owner] names the rule ([Rel/rule]) or function ([$func]). *)
type site = { owner : string; span : Location.t; hit : Location.t }

type target = Rel of string | Rule of string * string | Prem of site
type breakpoint = { number : int; target : target }

(** [Rel], [Rel/rule], or [file:line in owner]. *)
val string_of_target : target -> string

(** The spec file that [file] names, as its locations record it. A relative
    [file] may be any suffix of the recorded path; an absolute one names the
    file wherever it is reached from. Raises {!Error} when no file, or more than
    one, matches. *)
val spec_file : string -> string

(** The written premise at [line] of the spec file named by [file] (as in
    {!spec_file}), or else the next one in the same rule or clause. Raises
    {!Error} when there is none. *)
val premise_at : file:string -> line:int -> site

(** The absolute path of [file] with [.], [..] and symbolic links resolved, so
    that two paths to one file are equal. A file that cannot be found is only
    made absolute. *)
val canonical : string -> string

(** Adds a breakpoint at [target], or returns the one already there. Raises
    {!Error} when no relation or rule matches. *)
val add_breakpoint : target -> [ `New of breakpoint | `Existing of breakpoint ]

(** Raises {!Error} when there is no breakpoint with that number. *)
val delete_breakpoint : int -> unit

(** In the order they were set. *)
val breakpoints : unit -> breakpoint list

(** {1 Calls} *)

(** A call in progress. *)
type frame = {
  name : string;  (** [Rel], [Rel/rule] once a rule is tried, or [$func] *)
  call_site : Location.t;
  location : Location.t;  (** its current rule, clause, or definition *)
  premise : Location.t option;  (** the written premise it is at *)
  inputs : Il.Value.t list;
  env : VEnv.t;  (** the bindings in scope *)
}

(** The calls in progress, innermost first. *)
val frames : unit -> frame list

(** {1 Running} *)

(** What a finished call produced. *)
type call_result =
  | Failed
  | Rel_outputs of Il.Value.t list  (** empty when the relation just holds *)
  | Func_output of Il.Value.t

(** Why the run stopped. *)
type stop_reason =
  | Breakpoints of breakpoint list
      (** One breakpoint, or a rule's and one on its first written premise. *)
  | Stepped of Location.t  (** at this written premise *)
  | Returned of { name : string; result : call_result }
      (** The call being finished returned; {!frames} shows its caller. *)

(** How to go on from a stop. [Next] steps over calls; [Finish] runs until the
    current call returns. *)
type resume = Continue | Step | Next | Finish

module type FRONTEND = sig
  (** Called once the spec is loaded; the run starts when it returns. *)
  val before_run : unit -> unit

  (** Called whenever the run pauses, with the reason. The run stays paused
      until this returns, and then continues as the returned [resume] says. *)
  val stopped : stop_reason -> resume

  (** Called once the run has ended, whether it succeeded or failed. *)
  val after_run : unit -> unit
end

(** The handler that runs a session, driven by a frontend. *)
module Make (_ : FRONTEND) : Instrumentation_api.Handler.S
