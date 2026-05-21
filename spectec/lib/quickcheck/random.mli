(** Splittable pure-functional PRNG.

    Implements Haskell QuickCheck's [split :: StdGen -> (StdGen, StdGen)]
    semantics in OCaml. Split is the key operation because the Gen monad's bind
    must be able to fork PRNG state into two independent streams. *)

(** Immutable PRNG state, composed of two int seeds. *)
type t

(** [make seed] initializes a deterministic PRNG from [seed]. *)
val make : int -> t

(** [make_self_init ()] creates a non-deterministic PRNG. Uses OCaml stdlib's
    [Random.State.make_self_init] internally. *)
val make_self_init : unit -> t

(** [split r] returns two mutually independent child streams [(r1, r2)] from
    [r]. Key operation for Gen monad bind: supplies r1 to the left computation
    and r2 to the right. *)
val split : t -> t * t

(** Returns a uniform random bool. *)
val bool : t -> bool

(** [int ~lo ~hi r] returns a uniform integer in [[lo, hi]]. *)
val int : lo:int -> hi:int -> t -> int

(** [float ~lo ~hi r] returns a uniform float in [[lo, hi]]. *)
val float : lo:float -> hi:float -> t -> float

(** Returns a printable ASCII character (codes 32–126) uniformly. *)
val char : t -> char
