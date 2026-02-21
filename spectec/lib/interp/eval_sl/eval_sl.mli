open Common.Source

module Ctx : sig
  type t
end

exception Error of region * string

val run_relation_fresh :
  string ->
  Builtins.t ->
  Lang.Sl.spec ->
  string ->
  Lang.Il.Value.t list ->
  Ctx.t * Lang.Il.Value.t list
