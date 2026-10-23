(** Builtin function implementations and opaque type definitions supplied by a
    target. *)
module type Externals = sig
  val builtins : (string * Builtins.Define.t) list

  (** Each opaque syntax declaration must have exactly one type definition with
      the same name. *)
  val opaque_types : Lang.Il.Opaque.typdef_as_any list
end

module type S = sig
  include Externals

  (** [with_state f] initializes target state, runs [f], and restores the
      previous state when [f] returns or raises. *)
  val with_state : (unit -> 'a) -> 'a

  val is_impure_func : string -> bool
  val is_impure_rel : string -> bool
  val state_version : int ref
end
