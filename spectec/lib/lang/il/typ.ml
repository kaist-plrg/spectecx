open Types
open Common.Source

type t = typ
type t' = typ'

(* Constructors *)

let bool = BoolT
let nat = NumT `NatT
let int = NumT `IntT
let text = TextT
let func = FuncT

let var (tid : id') (targs : t list) : t' =
  VarT { synid = tid $ no_region; targs }

let tuple (typs : t list) : t' = TupleT typs
let opt (typ : t) : t' = IterT { typ; iter = Opt }
let list (typ : t) : t' = IterT { typ; iter = List }

let rec iterate (typ : t) (iters : iter list) : t =
  match iters with
  | [] -> typ
  | iter :: iters -> iterate (IterT { typ; iter } $ typ.at) iters
