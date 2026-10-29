(** Tracks which parts of the task input each value comes from.

    A tracker records, for each value, a set of {!Instrumentation_api.Origin.t}
    that over-approximates the input locations whose data the value was copied
    or computed from. It does not record which rules, clauses or conditions
    applied, so a value that only depends on an input through control flow can
    lack that input's origin. The sets are not exact dependency readsets.

    Propagation follows {!Instrumentation_api.Value_event}:
    - [Created] labels the value and every value it contains with the origin
      extended by its structural path.
    - [Derived], [Combined] and [Updated] add the origins of the sources to the
      result.
    - [Invoked] adds the union of the input origins to each output that has no
      origin, and to every value reached from it through values without origins.
      This covers values built inside builtins, including parts that a caller
      later projects. Values that already have origins keep them unchanged,
      together with their parts.
    - A cached [Invoked] adds the union of the current input origins to each
      output and every value it contains. Calls with content-equal inputs from
      different origins share the cached output values, so the shared values
      report the origins of every such call, and an input value that a cached
      output contains reports them as well.

    The union of a call's input origins also reaches outputs of defined
    functions and relations whose results have no origin, such as literals, so
    those report inputs they do not read.

    The tracker keys values by vid, so it relies on vids being distinct within a
    run. It clears its records when its session starts and at each [Test_start].
    Records stay readable after the session finishes. *)

type t

val make : unit -> t

(** The handler entry for an instrumentation configuration. Its handler observes
    IL, SL and PL runs and writes no output. *)
val handler_config : t -> Instrumentation_config.Handler_config.t

(** The recorded origins of [value], in {!Instrumentation_api.Origin.compare}
    order. Empty when the value was not traced to the input. *)
val origins : t -> value:Lang.Il.Value.t -> Instrumentation_api.Origin.t list
