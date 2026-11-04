open Common.Domain
open Common.Annotated
module Il = Lang.Il
open Envs.Make

(* Environments *)

(* Identifier type and dimension environment *)
module VEnv = MakeIdMap (Ctx.Typ)
module RTEnv = Envs.RTEnv
module TDEnv = Envs.Il.TDEnv

(* Relation types and type definitions of the whole spec *)

type env = { rtenv : RTEnv.t; tdenv : TDEnv.t }

(* Context for dataflow analysis of one rule or clause *)

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
          let tdenv = TDEnv.add synid (tparams, deftyp) env.tdenv in
          { env with tdenv }
      | Il.RelD { relid; reltyp; _ } ->
          let rtenv = RTEnv.add relid reltyp env.rtenv in
          { env with rtenv }
      | Il.DecD _ | Il.BuiltinDecD _ -> env)
    { rtenv = RTEnv.empty; tdenv = TDEnv.empty }
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

let find_typdef_opt (dctx : t) (tid : TId.t) : Envs.Il.Typdef.t option =
  TDEnv.find_opt tid dctx.env.tdenv

module Typ = Ctx.Typ
