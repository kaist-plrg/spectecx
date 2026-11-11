open Envs_make
module Typ = Typ
module Syntax = Syntax

(* Syntax definition environment *)

module SEnv = MakeTIdMap (Syntax)
