open Lang.Il

(** Calls global specification functions with the caller's cache and target
    state, without the caller's local bindings. The callback raises
    [Invalid_argument] if used after the builtin exits. *)
type t = id -> targ list -> value list -> value
