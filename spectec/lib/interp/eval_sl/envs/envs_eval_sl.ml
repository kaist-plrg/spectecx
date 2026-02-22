open Semantics.MakeEnv
module Rel = Rel
module Func = Func

(* Environments *)

(* Value environment *)

module VEnv = Dynamic.Envs.VEnv

(* Type definition environment *)

module TDEnv = Dynamic.Envs.TDEnv

(* Relation environment *)

module REnv = MakeRIdMap (Rel)

(* Definition environment *)

module FEnv = MakeFIdMap (Func)
