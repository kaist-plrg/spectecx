module Fresh = Fresh

module P4_values : sig
  val global : Lang.Il.Value.t
  val bare_name : string -> Lang.Il.Value.t
  val is_reject : Lang.Il.Value.t -> bool

  val bool_of_value :
    Common.Location.t -> Lang.Il.Value.t -> (bool, Builtins.Error.t) result
end

module Ebpf = Ebpf

val builtins : (string * Builtins.Define.t) list
