open Common.Domain
open Common.Annotated
module Il = Lang.Il
open Envs.Make

(* Environments *)

(* Identifier type and dimension environment *)
module VEnv = MakeIdMap (Typ)
module RTEnv = Envs.RTEnv
module SEnv = Envs.Il.SEnv

(* Relation types and syntax definitions for the whole spec *)

type env = { rtenv : RTEnv.t; senv : SEnv.t }

(* Context of the algo pass for one rule or clause *)

type t = {
  env : env;
  (* Identifiers the rule or clause uses, plus the fresh ones this pass adds *)
  frees : IdSet.t;
  (* Bound variables so far *)
  bounds : VEnv.t;
}

(* Constructors *)

let make_env (spec : Il.spec) : env =
  List.fold_left
    (fun env (def : Il.def) ->
      match def.it with
      | Il.TypD { synid; tparams; deftyp } ->
          let senv = SEnv.add synid (tparams, deftyp) env.senv in
          { env with senv }
      | Il.RelD { relid; reltyp; _ } ->
          let rtenv = RTEnv.add relid reltyp env.rtenv in
          { env with rtenv }
      | Il.DecD _ | Il.BuiltinDecD _ -> env)
    { rtenv = RTEnv.empty; senv = SEnv.empty }
    spec

let make (env : env) ~(frees : IdSet.t) : t =
  { env; frees; bounds = VEnv.empty }

(* Adders *)

let add_free (dctx : t) (id : Id.t) =
  let frees = IdSet.add id dctx.frees in
  { dctx with frees }

let add_bounds (dctx : t) (venv : VEnv.t) : t =
  let bounds = VEnv.union (fun _ -> assert false) dctx.bounds venv in
  { dctx with bounds }

(* Finders *)

let find_reltyp (dctx : t) (id : Id.t) : Il.reltyp =
  RTEnv.find id dctx.env.rtenv

let find_syntax_opt (dctx : t) (tid : TId.t) : Envs.Il.Syntax.t option =
  SEnv.find_opt tid dctx.env.senv

module Typ = Typ
