module Location = Common.Location
open Core
module H = Harness
module V = Lang.Il.Value
module Typ = Lang.Il.Typ

let ( $ ) = Common.Annotated.( $ )
let nat n = V.nat (Bigint.of_int n)
let nat_opt n = V.opt (Typ.nat $ Location.Unknown) (Option.map n ~f:nat)
let spec = H.compile_file "partial_binding.spectec"

let run () =
  H.check spec ~name:"bound option accepts equal payload"
    ~relation:"First_if_three"
    ~args:(fun () -> [ nat 1; nat_opt (Some 3) ])
    (H.returns (fun () -> [ nat 1 ]));
  H.check spec ~name:"bound option rejects different payload"
    ~relation:"First_if_three"
    ~args:(fun () -> [ nat 1; nat_opt (Some 5) ])
    H.fails;
  H.check spec ~name:"bound option rejects absence" ~relation:"First_if_three"
    ~args:(fun () -> [ nat 1; nat_opt None ])
    H.fails
