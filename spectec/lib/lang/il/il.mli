(** The IL Abstract Syntax Tree and API. This module is the main entry point for
    IL. *)

include module type of Types
include module type of Effects
module Print : module type of Print
module Eq : module type of Eq
module Free : module type of Free
module Utils : module type of Utils
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
