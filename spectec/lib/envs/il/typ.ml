open Common.Source
open Common.InternalError
open Envs_make
open Il
open Typ

(* Substitution of type variables *)

type theta = t TIdMap.t

let rec subst_typ (theta : theta) (typ : t) : t =
  match typ.it with
  | BoolT | NumT _ | TextT -> typ
  | VarT { synid; targs } -> (
      match TIdMap.find_opt synid theta with
      | Some typ ->
          if targs <> [] then
            disallowed typ.at
              ("higher-order substitution is disallowed for typ:"
             ^ Print.string_of_typ typ);
          typ
      | None ->
          let targs = subst_targs theta targs in
          VarT { synid; targs } $ typ.at)
  | TupleT typs ->
      let typs = subst_typs theta typs in
      TupleT typs $ typ.at
  | IterT { typ; iter } ->
      let typ = subst_typ theta typ in
      IterT { typ; iter } $ typ.at
  | FuncT -> typ

and subst_typs (theta : theta) (typs : t list) : t list =
  List.map (subst_typ theta) typs

and subst_targ (theta : theta) (targ : t) : t = subst_typ theta targ

and subst_targs (theta : theta) (targs : t list) : t list =
  List.map (subst_targ theta) targs

let subst_nottyp (theta : theta) (nottyp : nottyp) : nottyp =
  Mixfix.map (subst_typ theta) nottyp.it $ nottyp.at

let subst_typorigin (theta : theta) (typorigin : typorigin) : typorigin =
  let { synid; targs } = typorigin.it in
  let targs = subst_targs theta targs in
  { synid; targs } $ typorigin.at

let subst_typcase (theta : theta) (typcase : typcase) : typcase =
  let { notation; origin; hints } = typcase in
  let notation = subst_nottyp theta notation in
  let origin = subst_typorigin theta origin in
  { notation; origin; hints }

let rec subst_param (theta : theta) (param : param) : param =
  match param.it with
  | ExpP typ ->
      let typ = subst_typ theta typ in
      ExpP typ $ param.at
  (* (TODO) Capture-avoiding substitution *)
  | DefP { defid; tparams; params; typ } ->
      let params = subst_params theta params in
      let typ = subst_typ theta typ in
      DefP { defid; tparams; params; typ } $ param.at

and subst_params (theta : theta) (params : param list) : param list =
  List.map (subst_param theta) params
