(** Where a run can stop in a spec: the written premises of its rules and
    function clauses, found by file and line.

    A {e written} premise is one the spec's author wrote: an AL instruction
    whose origin is [Premise]. The algo pass synthesizes instructions with no
    source of their own: the guards that check a rule's or clause's input
    patterns, and the side conditions that check a premise's results. Leaving
    those out lets stepping and line breakpoints follow the spec as written. *)

module Il = Lang.Il
module Location = Common.Location

(** A written premise of a rule or clause. [span] is the premise as written;
    [hit] is where it is entered, which is inside [span] for an iterated
    premise. [owner] names the rule ([Rel/rule]) or function ([$func]). *)
type site = { owner : string; span : Location.t; hit : Location.t }

(** The rules and clauses of a spec, each with its written premises in order. *)
type t

val index : Lang.Al.spec -> t
val empty : t

(** Whether [prem] is written (see above). *)
val is_written : Lang.Al.instr -> bool

(** The absolute path of [file] with [.], [..] and symbolic links resolved, so
    that two paths to one file are equal. A file that cannot be found is only
    made absolute. *)
val canonical : string -> string

(** The spec file that [name] names, as its locations record it. A relative
    [name] may be any suffix of the recorded path; an absolute one names the
    file wherever it is reached from. An error says why when no file, or more
    than one, matches. *)
val file : t -> string -> (string, string) result

(** The written premise at [line] of [file] (as {!file} returns it), or else the
    next one in the same rule or clause. When several premises cover the line,
    the one starting last. An error says why when there is none. *)
val premise_at : t -> file:string -> line:int -> (site, string) result
