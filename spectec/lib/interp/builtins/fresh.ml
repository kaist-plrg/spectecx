module Il = Lang.Il
open Il
open Common.Source

(* dec $fresh_tid() : tid *)

let fresh_tid ~at : (Value.t, Error.t) result =
  at |> ignore;
  let tid = Effect.perform Effects.FreshTid () in
  let typ = VarT ("tid" $ no_region, []) in
  Ok (Il.Value.Make.text typ tid)

let builtins = [ ("fresh_tid", Define.T0.a0 fresh_tid) ]
