(** Events that report how values are built during an instrumented run.

    Handlers subscribe through {!Handler.S.handle_value} and identify values by
    [value.note.vid]. Values built within one [Interp.with_target_state] call
    have distinct [vid] fields. The counter restarts for each call.

    - {!Decoded}: decoded values whose origins a decoder reports.
    - {!Derived}: unary operators, length, type and pattern tests, casts that
      build new values, and values built while matching cons and iterated
      patterns.
    - {!Constructed}: tuple, case, struct, option and list constructor
      expressions.
    - {!Combined}: binary operators, comparisons, concatenation, membership,
      cons, text indexing, and text and list slicing. For each list or option
      produced by an iteration, the interpreter reports the lists or options it
      ranges over, followed by the produced elements.
    - {!Updated}: each container rebuilt along an update path.
    - {!Invoked}: successful calls, including builtins and cache hits.

    Interpreters emit no event for failed calls, literals, function references,
    variables, field access, list indexing, or casts that return their operand.
    Interpreters report each builtin call through [Invoked] but do not report
    how the builtin builds its outputs. *)

type callee = Func of string | Rel of string

type t =
  | Decoded of { value : Lang.Il.Value.t; origin : Origin.t }
      (** A decoder that reports origins must emit this event for every value it
          builds, including nested values. [origin] identifies the value by its
          input label and path. *)
  | Derived of { value : Lang.Il.Value.t; source : Lang.Il.Value.t }
      (** [value] was built from [source]. *)
  | Constructed of {
      value : Lang.Il.Value.t;
      components : Lang.Il.Value.t list;
    }  (** The values directly contained in [value] are [components]. *)
  | Combined of { value : Lang.Il.Value.t; sources : Lang.Il.Value.t list }
      (** [value] was computed from [sources]. *)
  | Updated of {
      value : Lang.Il.Value.t;
      base : Lang.Il.Value.t;
      replacement : Lang.Il.Value.t;
    }
      (** [value] is [base] with [replacement] written at one field, index or
          slice. The interpreter reuses unchanged list elements and struct field
          values. *)
  | Invoked of {
      callee : callee;
      inputs : Lang.Il.Value.t list;
      outputs : Lang.Il.Value.t list;
      cached : bool;
    }
      (** A call to [callee] returned [outputs]. A function has one output. When
          [cached] is true, the interpreter reuses output values from an earlier
          call with equal [inputs]. Value handlers read calls from this event
          because [Event.Func_exit] lacks the inputs and {!Event} records no
          cache hits. *)
