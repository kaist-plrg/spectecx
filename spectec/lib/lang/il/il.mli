(** The IL Abstract Syntax Tree and Core API. This module is the main entry
    point for IL. *)

include module type of Core.Types
include module type of Core.Effects
module Print : module type of Core.Print
module Eq : module type of Core.Eq
module Free : module type of Core.Free
module Utils : module type of Core.Utils
module Print_debug : module type of Core.Print_debug

(** Constructors and operations on IL Values. *)
module Value : sig
  include module type of Core.Value

  val to_string : t -> string
end

(** Constructors and operations on IL Types. *)
module Typ : sig
  include module type of Core.Typ

  val to_string : typ -> string
  val eq : typ -> typ -> bool
end
