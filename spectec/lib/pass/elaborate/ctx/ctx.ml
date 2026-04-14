open Common.Domain
open Common.Source
open Lang.El
module Il = Lang.Il
open Error
open Envs.Make

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Environments *)

(* Identifier type and dimension environment *)
module VEnv = MakeIdMap (Typ)

(* Meta-variable type environment (IL types) *)
module MetaTyp = struct
  type t = Il.typ

  let to_string = Il.Print.string_of_typ
end

module MEnv = MakeIdMap (MetaTyp)

(* Type definition environment *)
module TDEnv = MakeTIdMap (Typdef)

(* Hint environment *)
module HEnv = Envs.HEnv

(* Relation environment *)
module REnv = MakeRIdMap (Rel)

(* Definition environment *)
module FEnv = MakeFIdMap (Func)

(* Global counter for unique identifiers *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := !tick + 1;
  id

(* Context *)

type t = {
  (* Set of free ids, for unique id insertion *)
  frees : IdSet.t;
  (* Map from variable ids to dimensions *)
  venv : VEnv.t;
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from meta-type ids to meta-types *)
  menv : MEnv.t;
  (* Map from relation ids to relations *)
  renv : REnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
}

(* Constructors *)

let empty : t =
  {
    frees = IdSet.empty;
    venv = VEnv.empty;
    tdenv = TDEnv.empty;
    menv = MEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

let init () : t =
  let menv =
    MEnv.empty
    |> MEnv.add ("bool" $ no_region) (Il.BoolT $ no_region)
    |> MEnv.add ("nat" $ no_region) (Il.NumT `NatT $ no_region)
    |> MEnv.add ("int" $ no_region) (Il.NumT `IntT $ no_region)
    |> MEnv.add ("text" $ no_region) (Il.TextT $ no_region)
  in
  { empty with menv }

(* Finders *)

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  TDEnv.find_opt tid ctx.tdenv

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> error_undef tid.at "type" tid.it

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for meta-variables *)

let find_metavar_opt (ctx : t) (tid : TId.t) : Il.typ option =
  MEnv.find_opt tid ctx.menv

let find_metavar (ctx : t) (tid : TId.t) : Il.typ =
  match find_metavar_opt ctx tid with
  | Some typ -> typ
  | None -> error_undef tid.at "meta-variable" tid.it

let bound_metavar (ctx : t) (tid : TId.t) : bool =
  find_metavar_opt ctx tid |> Option.is_some

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : (Il.nottyp * int list) option =
  REnv.find_opt rid ctx.renv
  |> Option.map (fun (nottyp, inputs, _) -> (nottyp, inputs))

let find_rel (ctx : t) (rid : RId.t) : Il.nottyp * int list =
  match find_rel_opt ctx rid with
  | Some (nottyp, inputs) -> (nottyp, inputs)
  | None -> error_undef rid.at "relation" rid.it

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_opt ctx rid |> Option.is_some

let find_rules_opt (ctx : t) (rid : RId.t) : Il.rule list option =
  REnv.find_opt rid ctx.renv |> Option.map (fun (_, _, rules) -> rules)

let find_rules (ctx : t) (rid : RId.t) : Il.rule list =
  match find_rules_opt ctx rid with
  | Some rules -> rules
  | None -> error_undef rid.at "relation" rid.it

(* Finders for definitions *)

let find_defined_dec_opt (ctx : t) (fid : FId.t) :
    (Il.tparam list * Il.param list * Il.typ * Il.clause list) option =
  match FEnv.find_opt fid ctx.fenv with
  | Some (Func.Defined (tparams, params, typ, clauses)) ->
      Some (tparams, params, typ, clauses)
  | Some (Func.Builtin _) | None -> None

let find_defined_dec (ctx : t) (fid : FId.t) :
    Il.tparam list * Il.param list * Il.typ * Il.clause list =
  match find_defined_dec_opt ctx fid with
  | Some result -> result
  | None -> error_undef fid.at "defined dec" fid.it

let bound_defined_dec (ctx : t) (fid : FId.t) : bool =
  find_defined_dec_opt ctx fid |> Option.is_some

let find_dec_signature_opt (ctx : t) (fid : FId.t) :
    (Il.tparam list * Il.param list * Il.typ) option =
  FEnv.find_opt fid ctx.fenv
  |> Option.map (function
         | Func.Builtin (tparams, params, typ)
         | Func.Defined (tparams, params, typ, _)
         -> (tparams, params, typ))

let find_dec_signature (ctx : t) (fid : FId.t) :
    Il.tparam list * Il.param list * Il.typ =
  match find_dec_signature_opt ctx fid with
  | Some result -> result
  | None -> error_undef fid.at "dec" fid.it

let bound_dec (ctx : t) (fid : FId.t) : bool =
  find_dec_signature_opt ctx fid |> Option.is_some

(* Adders *)

(* Adders for free variables *)

let add_free (ctx : t) (id : Id.t) : t =
  let frees = IdSet.add id ctx.frees in
  { ctx with frees }

let add_frees (ctx : t) (ids : IdSet.t) : t =
  ids |> IdSet.elements |> List.fold_left (fun ctx id -> add_free ctx id) ctx

(* Adders for meta-variables *)

let add_metavar (ctx : t) (tid : TId.t) (typ : Il.typ) : t =
  if bound_metavar ctx tid then error_dup tid.at "meta-variable" tid.it;
  let menv = MEnv.add tid typ ctx.menv in
  { ctx with menv }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then error_dup tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparam (ctx : t) (tparam : tparam) : t =
  let ctx = add_typdef ctx tparam Typdef.Param in
  add_metavar ctx tparam (Il.VarT (tparam, []) $ tparam.at)

let add_tparams (ctx : t) (tparams : tparam list) : t =
  List.fold_left add_tparam ctx tparams

(* Adders for rules *)

let add_rel (ctx : t) (rid : RId.t) (nottyp : Il.nottyp) (inputs : int list) : t
    =
  if bound_rel ctx rid then error_dup rid.at "relation" rid.it;
  let rel = (nottyp, inputs, []) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_rule (ctx : t) (rid : RId.t) (rule : Il.rule) : t =
  if not (bound_rel ctx rid) then error_undef rid.at "relation" rid.it;
  let nottyp, inputs, rules = REnv.find rid ctx.renv in
  let rel = (nottyp, inputs, rules @ [ rule ]) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

(* Adders for definitions *)

let add_builtin_dec (ctx : t) (fid : FId.t) (tparams : Il.tparam list)
    (params : Il.param list) (typ : Il.typ) : t =
  if bound_dec ctx fid then error_dup fid.at "dec" fid.it;
  let func = Func.Builtin (tparams, params, typ) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_defined_dec (ctx : t) (fid : FId.t) (tparams : Il.tparam list)
    (params : Il.param list) (typ : Il.typ) : t =
  if bound_dec ctx fid then error_dup fid.at "dec" fid.it;
  let func = Func.Defined (tparams, params, typ, []) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_defined_clause (ctx : t) (fid : FId.t) (clause : Il.clause) : t =
  if not (bound_defined_dec ctx fid) then
    error_undef clause.at "defined dec" fid.it;
  let tparams, params, typ, clauses = find_defined_dec ctx fid in
  let func = Func.Defined (tparams, params, typ, clauses @ [ clause ]) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if not (bound_typdef ctx tid) then error_undef tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

module Typ = Typ
module Typdef = Typdef
module Func = Func
module Rel = Rel
