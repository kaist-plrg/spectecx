open Core
module H = Harness
module V = Lang.Il.Value
module Typ = Lang.Il.Typ

let ( $ ) = Common.Source.( $ )
let no_region = Common.Source.no_region
let nat_type = Typ.nat $ no_region
let nat n = V.nat (Bigint.of_int n)
let nats ns = V.list nat_type (List.map ns ~f:nat)

let nat_lists nss =
  V.list (Typ.list nat_type $ no_region) (List.map nss ~f:nats)

let spec = H.compile_file "iteration.spectec"

let run () =
  H.check spec ~name:"iterated relation binds each output" ~relation:"Add_each"
    ~args:(fun () -> [ nats [ 1; 2; 3 ]; nats [ 10; 20; 30 ] ])
    (H.returns (fun () -> [ nats [ 11; 22; 33 ] ]));
  H.check spec ~name:"iterated relation accepts empty inputs"
    ~relation:"Add_each"
    ~args:(fun () -> [ nats []; nats [] ])
    (H.returns (fun () -> [ nats [] ]));
  H.check spec ~name:"iterated relation rejects different input lengths"
    ~relation:"Add_each"
    ~args:(fun () -> [ nats [ 1; 2 ]; nats [ 10 ] ])
    (H.fails_with "cannot transpose a matrix of value batches");
  H.check spec ~name:"nested relation keeps empty inner lists"
    ~relation:"Add_nested"
    ~args:(fun () ->
      [
        nat_lists [ [ 1; 2 ]; []; [ 3 ] ]; nat_lists [ [ 10; 20 ]; []; [ 30 ] ];
      ])
    (H.returns (fun () -> [ nat_lists [ [ 11; 22 ]; []; [ 33 ] ] ]));
  H.check spec ~name:"nested relation rejects different inner lengths"
    ~relation:"Add_nested"
    ~args:(fun () -> [ nat_lists [ [ 1; 2 ] ]; nat_lists [ [ 10 ] ] ])
    (H.fails_with "cannot transpose a matrix of value batches");
  H.check spec ~name:"iterated binding binds each output"
    ~relation:"Increment_each"
    ~args:(fun () -> [ nats [ 1; 2 ] ])
    (H.returns (fun () -> [ nats [ 2; 3 ] ]));
  H.check spec ~name:"iterated binding accepts empty input"
    ~relation:"Increment_each"
    ~args:(fun () -> [ nats [] ])
    (H.returns (fun () -> [ nats [] ]));
  H.check spec ~name:"iterated condition accepts all true"
    ~relation:"Require_positive_each"
    ~args:(fun () -> [ nats [ 1; 2 ] ])
    (H.returns (fun () -> [ nats [ 1; 2 ] ]));
  H.check spec ~name:"iterated condition rejects one false"
    ~relation:"Require_positive_each"
    ~args:(fun () -> [ nats [ 1; 0; 2 ] ])
    H.fails;
  H.check spec ~name:"nested condition accepts empty inner lists"
    ~relation:"Require_positive_nested"
    ~args:(fun () -> [ nat_lists [ [ 1 ]; []; [ 2 ] ] ])
    (H.returns (fun () -> [ nat_lists [ [ 1 ]; []; [ 2 ] ] ]));
  H.check spec ~name:"nested condition rejects one false"
    ~relation:"Require_positive_nested"
    ~args:(fun () -> [ nat_lists [ [ 1 ]; [ 2; 0 ] ] ])
    H.fails;
  H.check spec ~name:"iterated assertion accepts all successes"
    ~relation:"Assert_positive_each"
    ~args:(fun () -> [ nats [ 1; 2 ] ])
    (H.returns (fun () -> [ nats [ 1; 2 ] ]));
  H.check spec ~name:"iterated assertion rejects one failure"
    ~relation:"Assert_positive_each"
    ~args:(fun () -> [ nats [ 1; 0 ] ])
    H.fails
