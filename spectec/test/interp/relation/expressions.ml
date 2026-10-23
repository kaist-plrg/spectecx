open Core
module H = Harness
module V = Lang.Il.Value

let nat n = V.nat (Bigint.of_int n)

let nats ns =
  V.list Common.Source.(Lang.Il.Typ.nat $ no_region) (List.map ns ~f:nat)

let spec = H.compile_file "expressions.spectec"

let run () =
  H.check spec ~name:"Boolean operators evaluate the right operand"
    ~relation:"Eager"
    ~args:(fun () -> [ V.bool true ])
    (H.fails_with "builtin $boom is declared in the spec but not implemented");
  H.check spec ~name:"binary operands evaluate left to right"
    ~relation:"Ordered"
    ~args:(fun () -> [ nat 0 ])
    (H.fails_with "builtin $first is declared in the spec but not implemented");
  H.check spec ~name:"function returns indexed list update" ~relation:"Replace"
    ~args:(fun () -> [ nats [ 2; 4; 6 ]; nat 1; nat 9 ])
    (H.returns (fun () -> [ nats [ 2; 9; 6 ] ]));
  H.check spec ~name:"indexed update reports bounds" ~relation:"Replace"
    ~args:(fun () -> [ nats [ 2; 4; 6 ]; nat 3; nat 9 ])
    (H.fails_with "index 3 out of bounds [0, 3)");
  H.check spec ~name:"slice uses start and length" ~relation:"Slice"
    ~args:(fun () -> [ V.text "abcde"; nat 1; nat 2 ])
    (H.returns (fun () -> [ V.text "bc" ]));
  H.check spec ~name:"empty slice at end" ~relation:"Slice"
    ~args:(fun () -> [ V.text "abcde"; nat 5; nat 0 ])
    (H.returns (fun () -> [ V.text "" ]));
  H.check spec ~name:"slice reports bounds" ~relation:"Slice"
    ~args:(fun () -> [ V.text "abcde"; nat 4; nat 2 ])
    (H.fails_with "slice [4, 6) out of bounds [0, 5)");
  H.check spec ~name:"slice replacement retains surrounding text"
    ~relation:"Replace_slice"
    ~args:(fun () -> [ V.text "abcde"; nat 1; nat 2; V.text "XY" ])
    (H.returns (fun () -> [ V.text "aXYde" ]));
  H.check spec ~name:"slice replacement requires matching length"
    ~relation:"Replace_slice"
    ~args:(fun () -> [ V.text "abcde"; nat 1; nat 2; V.text "X" ])
    (H.fails_with "updating a slice of length 2 requires a text of length 1");
  H.check spec ~name:"empty list length" ~relation:"Size"
    ~args:(fun () -> [ nats [] ])
    (H.returns (fun () -> [ nat 0 ]))
