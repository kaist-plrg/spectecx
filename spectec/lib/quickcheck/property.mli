(** Result types and the Testable interface for property-based testing.

    Direct translation of the Haskell spec from goal.md:
    - [Result] = [{ ok :: Maybe Bool; stamp :: [String]; arguments :: [String] }]
    - [Property] = [Prop (Gen Result)]
    - [TESTABLE] = [class Testable a where property :: a -> Property] *)

(** {2 Test results} *)

module Result : sig
  type t = {
    ok : bool option;
    (** [None] = neutral/discarded, [Some true] = pass, [Some false] = fail *)
    stamp : string list;
    (** Labels accumulated by [label]/[classify]. Used for statistics. *)
    arguments : string list;
    (** String representation of counterexample arguments recorded by [for_all]. *)
    shrink : unit -> t Gen.t list;
    (** Lazy thunk: returns candidate generators to try for shrinking.
        Populated by [for_all ~shrink]; evaluated and run by [Test.check]. *)
    generalize : unit -> (string * t Gen.t list) list;
    (** Lazy thunk: returns generalization candidates as [(label, samples)] pairs.
        A candidate is accepted when ALL samples give [ok = Some false].
        Populated by [for_all ~generalize]; applied by [Test.check] after shrinking. *)
  }

  val nothing : t
  (** Default neutral result: [{ ok = None; stamp = []; arguments = [] }]. *)

  val with_ok : bool -> t
  (** [with_ok b] is [{ nothing with ok = Some b }]. *)

  val add_argument : string -> t -> t
  val add_stamp : string -> t -> t
end

(** {2 Property type} *)

type t = Prop of Result.t Gen.t
(** [Property] is a generator of [Result]. *)

(** Alias to refer to the outer [t] from within a nested module type. *)
type prop = t

val of_result : Result.t -> t
(** Lifts a fixed result into a property. *)

val evaluate : t -> Result.t Gen.t
(** Extracts the internal generator. *)

(** {2 Testable type class} *)

module type TESTABLE = sig
  type t
  val property : t -> prop
  (** [property x] converts [x] to a [Property]. *)
end

(** {2 Primitive Testable instances} *)

module Bool_testable : TESTABLE with type t = bool
module Prop_testable : TESTABLE with type t = prop

(** {2 Derived Testable Functor} *)

module Make_fun_testable (A : Arbitrary.ARBITRARY) (B : TESTABLE) :
  TESTABLE with type t = A.t -> B.t
(** Derives [Testable (a -> b)] from [Arbitrary a] and [Testable b].
    Direct translation of Haskell's [property f = forAll arbitrary f]. *)

(** {2 Property combinators} *)

val for_all :
  ?shrink:('a -> 'a list) ->
  ?generalize:('a -> (string * 'a Gen.t) list) ->
  show:('a -> string) ->
  'a Gen.t ->
  ('a -> t) ->
  t
(** [for_all ?shrink ?generalize ~show gen body] generates a value with [gen],
    supplies it to [body], and records the string representation in [arguments].
    If [shrink] is provided, [Test.check] uses it to find a minimal counterexample.
    If [generalize] is provided, [Test.check] tries each [(label, gen')] candidate
    after shrinking: a candidate is accepted when ALL [generalize_n] samples drawn
    from [gen'] still give [ok = Some false], replacing the argument with [label]. *)

val ( ==> ) : bool -> t -> t
(** [cond ==> prop]: if [cond] is [true] returns [prop], otherwise returns neutral. *)

val label : string -> t -> t
(** [label s prop] adds [s] to the [stamp] of all results. *)

val classify : bool -> string -> t -> t
(** [classify true name prop = label name prop],
    [classify false _ prop = prop]. *)

val collect : show:('a -> string) -> 'a -> t -> t
(** [collect ~show v prop = label (show v) prop]. *)
