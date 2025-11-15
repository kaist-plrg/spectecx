open Types
open Util.Source
open Domain.Lib
open Util.InternalError

type t = typ
type t' = typ'

(* Constructors *)

let bool = BoolT
let nat = NumT `NatT
let int = NumT `IntT
let text = TextT
let func = FuncT
let var (tid : id') (targs : t list) : t' = VarT (tid $ no_region, targs)
let tuple (typs : t list) : t' = TupleT typs
let opt (typ : t) : t' = IterT (typ, Opt)
let list (typ : t) : t' = IterT (typ, List)

let rec iterate (typ : t) (iters : iter list) : t =
  match iters with
  | [] -> typ
  | iter :: iters -> iterate (IterT (typ, iter) $ typ.at) iters

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
