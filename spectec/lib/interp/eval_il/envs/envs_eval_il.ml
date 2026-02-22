open Semantics.MakeEnv
module Rel = Rel
module Func = Func

(* Environments *)

(* Global layer *)
module Global = struct
  (* Type definition environment *)
  module TDEnv = MakeFrozenTIdTbl (Dynamic.Typdef)

  (* Relation environment *)
  module REnv = MakeFrozenRIdTbl (Rel)

  (* Function environment *)
  module FEnv = MakeFrozenFIdTbl (Func)
end

(* Local layer *)
module Local = struct
  (* Type definition environment *)
  module TDEnv = Dynamic.Envs.TDEnv

  (* Function environment *)
  module FEnv = MakeFIdMap (Func)

  (* Value environment *)
  module VEnv = Dynamic.Envs.VEnv
end
