open Common.Domain
open Ctx
module RTEnv = Envs.RTEnv

(* Context for dataflow analysis *)

type t = {
  reltyps : RTEnv.t;
  (* Free identifiers over the entire definition *)
  frees : IdSet.t;
  (* Bound variables so far *)
  bounds : VEnv.t;
  (* Typedefs so far *)
  tdenv : TDEnv.t;
}

(* Constructors *)

let init (ctx : Ctx.t) : t =
  let reltyps = REnv.map (fun (reltyp, _) -> reltyp) ctx.renv in
  let frees = ctx.frees in
  let bounds = ctx.venv in
  let tdenv = ctx.tdenv in
  { reltyps; frees; bounds; tdenv }

(* Promoter *)

let promote (ctx : Ctx.t) (dctx : t) (venv : VEnv.t) : Ctx.t =
  let frees = dctx.frees in
  let venv = VEnv.union (fun _ -> assert false) ctx.venv venv in
  { ctx with frees; venv }

(* Adders *)

let add_free (dctx : t) (id : Id.t) =
  let frees = IdSet.add id dctx.frees in
  { dctx with frees }

(* Finders *)

let find_reltyp (dctx : t) (id : Id.t) = RTEnv.find id dctx.reltyps
let find_typdef (dctx : t) (tid : TId.t) = TDEnv.find tid dctx.tdenv
