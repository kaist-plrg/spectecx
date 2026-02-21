module Il = Lang.Il
open Il
open Common.Source
open Builtins
open Error

(* Global tid provider for P4 *)
module GlobalTidProvider = struct
  let provider : (unit -> string) ref = ref (fun () -> "FRESH__0")
  let set (p : unit -> string) = provider := p
  let reset () = provider := fun () -> "FRESH__0"
  let fresh () = !provider ()
end

(* dec $fresh_tid() : tid *)
let fresh_tid ~at : Value.t result =
  at |> ignore;
  let tid = GlobalTidProvider.fresh () in
  let typ = VarT ("tid" $ no_region, []) in
  Ok (Il.Value.Make.text typ tid)

let builtins = [ ("fresh_tid", Define.T0.a0 fresh_tid) ]
