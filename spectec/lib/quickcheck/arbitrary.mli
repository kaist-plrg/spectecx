(** Arbitrary / Coarbitrary type class simulation.

    Encodes Haskell type classes as OCaml module types ([ARBITRARY],
    [COARBITRARY]) and derives composite instances via Functors
    ([Make_list], [Make_pair], etc.). *)

(** {2 Type class signatures} *)

module type ARBITRARY = sig
  type t
  val arbitrary : t Gen.t
  (** [arbitrary] is the default generator for type [t]. *)
end

module type COARBITRARY = sig
  type t
  val coarbitrary : t -> 'b Gen.t -> 'b Gen.t
  (** [coarbitrary x gen] perturbs the PRNG state of [gen] based on the
      structure of [x]. Used to build function generators ([arbitrary (a -> b)]). *)
end

(** {2 Primitive instances} *)

module Bool : sig
  include ARBITRARY with type t = bool
  val coarbitrary : bool -> 'b Gen.t -> 'b Gen.t
end

module Nat : sig
  (** Non-negative integers. Upper bound limited by the size parameter. *)
  include ARBITRARY with type t = int
  val coarbitrary : int -> 'b Gen.t -> 'b Gen.t
end

module Int : sig
  (** Signed integers. Generated in the [-size, size] range. *)
  include ARBITRARY with type t = int
  val coarbitrary : int -> 'b Gen.t -> 'b Gen.t
end

module Char : sig
  include ARBITRARY with type t = char
  val coarbitrary : char -> 'b Gen.t -> 'b Gen.t
end

module Text : sig
  include ARBITRARY with type t = string
  val coarbitrary : string -> 'b Gen.t -> 'b Gen.t
end

(** {2 Derived instances (Functor)} *)

module Make_list (A : ARBITRARY) : ARBITRARY with type t = A.t list
(** Derives a list instance. Length is bounded by the size parameter. *)

module Make_option (A : ARBITRARY) : ARBITRARY with type t = A.t option
(** Derives an option instance. *)

module Make_pair (A : ARBITRARY) (B : ARBITRARY) :
  ARBITRARY with type t = A.t * B.t
(** Derives a pair instance. *)

module Make_fun (A : COARBITRARY) (B : ARBITRARY) :
  ARBITRARY with type t = A.t -> B.t
(** Derives a function instance.
    Direct translation of Haskell's [arbitrary = promote (`coarbitrary` arbitrary)]. *)

(** {2 First-class module helper} *)

type 'a arbitrary = (module ARBITRARY with type t = 'a)

val gen_of : 'a arbitrary -> 'a Gen.t
(** Extracts the generator from [(module M)]. *)
