(** Gen monad: size-aware random value generator.

    Direct OCaml translation of Haskell QuickCheck's
    [newtype Gen a = Gen (Int -> Rand -> a)]. The size parameter ([int])
    bounds the complexity of generated values, and [Random.t] carries the
    PRNG state. Both are propagated implicitly through bind. *)

type 'a t
(** Generator for ['a] values. Internally represented as [int -> Random.t -> 'a]. *)

(** {2 Monad interface} *)

val return : 'a -> 'a t
(** [return a] is a constant generator that always produces [a]. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** Monadic bind: splits the PRNG state and supplies an independent stream
    to the continuation. Direct translation of [>>=] from goal.md. *)

val map : ('a -> 'b) -> 'a t -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [let*] is [bind]. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** [let+] is [map]. *)

val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
(** [and*] runs two generators independently by splitting the PRNG, producing a pair. *)

(** {2 Execution} *)

val run : 'a t -> size:int -> rand:Random.t -> 'a
(** [run gen ~size ~rand] executes the generator. *)

val sample : 'a t -> 'a
(** [sample gen] runs with default size (5) and a self-initialized PRNG.
    Useful for debugging and interactive exploration. *)

val of_fun : (int -> Random.t -> 'a) -> 'a t
(** [of_fun f] wraps function [f] as a generator.
    Used to construct generators directly from raw functions. *)

(** {2 Core combinators} *)

val sized : (int -> 'a t) -> 'a t
(** [sized f] passes the current size parameter to [f]. *)

val resize : int -> 'a t -> 'a t
(** [resize n gen] runs [gen] with size fixed to [n]. *)

val scale : (int -> int) -> 'a t -> 'a t
(** [scale f gen] transforms the current size with [f] before running [gen]. *)

val choose_int : int * int -> int t
(** [choose_int (lo, hi)] generates a uniform integer in [[lo, hi]]. *)

val elements : 'a list -> 'a t
(** [elements xs] picks one element from [xs] uniformly.
    Raises [Invalid_argument] on an empty list. *)

val oneof : 'a t list -> 'a t
(** [oneof gens] picks and runs one generator from the list uniformly. *)

val frequency : (int * 'a t) list -> 'a t
(** [frequency weighted] selects a generator proportionally to its weight. *)

val variant : int -> 'a t -> 'a t
(** [variant v gen] perturbs the PRNG state by [v] before running [gen].
    Used by [coarbitrary] to build function generators.
    Implements the [rands r !! (v+1)] pattern from goal.md. *)

val promote : ('a -> 'b t) -> ('a -> 'b) t
(** [promote f] lifts a generator-returning function into a function-producing
    generator. Used to implement the Haskell arbitrary(a -> b) instance. *)

val list_of : ?min:int -> 'a t -> 'a list t
(** [list_of gen] generates a list whose length is bounded by the size parameter.
    [~min] specifies the minimum length (default 0). *)

val option_of : 'a t -> 'a option t
(** [option_of gen] generates [None] or [Some x] with equal probability. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair ga gb] runs two generators independently and produces a pair. *)

val sequence : 'a t list -> 'a list t
(** [sequence gens] runs a list of generators in order and collects the results. *)
