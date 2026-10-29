(** Events that report how interpreters produce values.

    A handler subscribes through {!Handler.S.observe_value}. Observers return
    unit, so they can record facts about values but cannot change what the
    interpreter computes. IL, SL and PL report the same events, because the
    shared expression evaluator in [Interp_runtime.Expression] emits all events
    for expressions, and each interpreter emits {!Invoked} from its call path.
    Interpreters build an event only when some observer subscribed.

    Observers identify values by [value.note.vid]. Vids are distinct within one
    run when the target's [with_state] installs a fresh vid counter, as every
    in-tree target does. A new run, or a new test case in a batch, can reuse the
    vids of an earlier one.

    {2 Coverage}

    - No event: literals, function references, and forms that return a value
      already held elsewhere, namely variables, struct field access, list
      indexing, and a cast that leaves its operand unchanged. Such a result
      keeps its identity.
    - {!Derived}: unary operators, length, type tests ([SubE]), pattern tests
      ([MatchE]), and each level of a cast that builds a new value. Pattern
      bindings report the values they build, such as the tail of a cons pattern
      and the lists or options bound by an iterated pattern, as derived from the
      matched value.
    - {!Combined}: binary operators, comparisons, concatenation, membership,
      cons, text indexing and slicing (with every operand as a source), and the
      constructors of tuples, cases, structs, options and lists (with their
      components as sources). An iteration result, from an iterated expression
      or the bindings of an iterated premise (an iterated let or relation
      instruction in SL and PL), combines the lists or options that the
      iteration ranged over with the values it produced, so it also reflects
      values that the iterated body reads from outside the iteration.
    - {!Updated}: each container rebuilt along the path of an update expression.
    - {!Invoked}: each successful function or relation call, including builtins
      and calls answered from the cache.

    Not reported: values built inside builtins, failed calls, and values the
    interpreters build only to decide control flow, such as the results of
    iterated conditions. *)

type callee = Func of string | Rel of string

type t =
  | Created of { value : Lang.Il.Value.t; origin : Origin.t }
      (** [value] was decoded from an external input at [origin]. Decoders, such
          as a task's [parse_input], emit this event. Sub-values of [value] are
          not reported separately. *)
  | Derived of { value : Lang.Il.Value.t; source : Lang.Il.Value.t }
      (** [value] was built from one existing value. *)
  | Combined of { value : Lang.Il.Value.t; sources : Lang.Il.Value.t list }
      (** [value] was built from [sources], which may be empty for an empty
          constructor. *)
  | Updated of {
      value : Lang.Il.Value.t;
      base : Lang.Il.Value.t;
      element : Lang.Il.Value.t;
    }
      (** [value] is [base] with [element] written at one field, index or slice.
          Other components of [value] are the same values as in [base]. *)
  | Invoked of {
      callee : callee;
      inputs : Lang.Il.Value.t list;
      outputs : Lang.Il.Value.t list;
      cached : bool;
    }
      (** A call to [callee] returned [outputs]. A function has one output. With
          [cached], the outputs are the values an earlier call with equal
          [inputs] returned, so they keep the identity of that call's results.
      *)
