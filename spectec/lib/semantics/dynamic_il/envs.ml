open Common.Domain

(* Environments *)

(* Value environment *)

module VEnv = Dynamic.Envs.VEnv

(* Type definition environment *)

module TDEnv = Dynamic.Envs.TDEnv

(* Relation environment *)

module REnv = MakeRIdEnv (Rel)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
