open Il
open Util.Source

(* dec $fresh_tid() : tid *)

let fresh_tid ~at : (Value.t, Err.t) result =
  at |> ignore;
  let tid = Effect.perform FreshTid () in
  let typ = Il.VarT ("tid" $ no_region, []) in
  Ok (Value.Make.text typ tid)

let builtins = [ ("fresh_tid", Define.T0.a0 fresh_tid) ]
