open Semantics.MakeEnv

(* Environments *)

(* Identifier type and dimension environment *)

module VEnv = MakeIdMap (Typ)

(* Plain type (EL type) environment *)

module PTEnv = MakeIdMap (Plaintyp)

(* Type definition environment *)

module TDEnv = MakeTIdMap (Typdef)

(* Relation environment *)

module HEnv = Semantics.HEnv
module REnv = MakeRIdMap (Rel)

(* Definition environment *)

module FEnv = MakeFIdMap (Func)
