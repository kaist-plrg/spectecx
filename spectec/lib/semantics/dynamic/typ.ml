open Common.Source
open Common.InternalError
open Common.Domain
open Lang.Il
open Typ

(* Substitution of type variables *)

type theta = t TIdMap.t

let rec subst_typ (theta : theta) (typ : t) : t =
  match typ.it with
  | BoolT | NumT _ | TextT -> typ
  | VarT (tid, targs) -> (
      match TIdMap.find_opt tid theta with
      | Some typ ->
          if targs <> [] then
            disallowed typ.at
              ("higher-order substitution is disallowed for typ:"
             ^ Print.string_of_typ typ);
          typ
      | None ->
          let targs = subst_targs theta targs in
          VarT (tid, targs) $ typ.at)
  | TupleT typs ->
      let typs = subst_typs theta typs in
      TupleT typs $ typ.at
  | IterT (typ, iter) ->
      let typ = subst_typ theta typ in
      IterT (typ, iter) $ typ.at
  | FuncT -> typ

and subst_typs (theta : theta) (typs : t list) : t list =
  List.map (subst_typ theta) typs

and subst_targ (theta : theta) (targ : t) : t = subst_typ theta targ

and subst_targs (theta : theta) (targs : t list) : t list =
  List.map (subst_targ theta) targs
