(** Data types and pure data functions for IL. IL is the internal representation
    after elaboration. *)

include module type of Types
module Effects : module type of Effects
module Eq : module type of Eq
module Free : module type of Free
module Utils : module type of Utils
module Print : module type of Print
module Print_debug : module type of Print_debug

(** Constructors and operations on IL Values. *)
module Value : sig
  include module type of Value

  val to_string : t -> string
end

(** Constructors and operations on IL Types. *)
module Typ : sig
  include module type of Typ

  val to_string : typ -> string
  val eq : typ -> typ -> bool
end
