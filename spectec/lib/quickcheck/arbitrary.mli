(** Arbitrary type class simulation.

    Encodes the Haskell [Arbitrary] type class as an OCaml module type. *)

(** {2 Type class signature} *)

module type ARBITRARY = sig
  type t
  val arbitrary : t Gen.t
  (** [arbitrary] is the default generator for type [t]. *)
end

(** {2 Primitive instances} *)

module Bool : ARBITRARY with type t = bool

module Text : ARBITRARY with type t = string

(** {2 First-class module helper} *)

type 'a arbitrary = (module ARBITRARY with type t = 'a)

val gen_of : 'a arbitrary -> 'a Gen.t
(** Extracts the generator from [(module M)]. *)
