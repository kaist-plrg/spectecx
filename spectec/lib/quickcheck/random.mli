(** Splittable pure-functional PRNG.

    Implements Haskell QuickCheck's [split :: StdGen -> (StdGen, StdGen)]
    semantics in OCaml. Split is the key operation because the Gen monad's
    bind must be able to fork PRNG state into two independent streams. *)

type t
(** Immutable PRNG state, composed of two int seeds. *)

val make : int -> t
(** [make seed] initializes a deterministic PRNG from [seed]. *)

val make_self_init : unit -> t
(** [make_self_init ()] creates a non-deterministic PRNG.
    Uses OCaml stdlib's [Random.State.make_self_init] internally. *)

val split : t -> t * t
(** [split r] returns two mutually independent child streams [(r1, r2)] from [r].
    Key operation for Gen monad bind: supplies r1 to the left computation
    and r2 to the right. *)

val bool : t -> bool
(** Returns a uniform random bool. *)

val int : lo:int -> hi:int -> t -> int
(** [int ~lo ~hi r] returns a uniform integer in [[lo, hi]]. *)

val float : lo:float -> hi:float -> t -> float
(** [float ~lo ~hi r] returns a uniform float in [[lo, hi]]. *)

val char : t -> char
(** Returns a printable ASCII character (codes 32–126) uniformly. *)
