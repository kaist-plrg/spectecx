module H = Harness
module V = Lang.Il.Value

let spec = H.compile_file "structuring.spectec"
let nat n = V.nat (Bigint.of_int n)

let run () =
  List.iter
    (fun (n, expected) ->
      H.check spec
        ~name:("merged case returns " ^ expected)
        ~relation:"Describe"
        ~args:(fun () -> [ nat n ])
        (H.returns (fun () -> [ V.text expected ])))
    [ (2, "even"); (3, "small odd"); (11, "large odd") ];
  H.check spec ~name:"shared predicate rejects zero" ~relation:"Describe"
    ~args:(fun () -> [ nat 0 ])
    H.fails;
  H.check spec ~name:"different relation inputs reject unmatched rules"
    ~relation:"Pick"
    ~args:(fun () -> [ nat 7; nat 3 ])
    H.fails;
  H.check spec
    ~name:"shared expression input preserves the second output binding"
    ~relation:"Size"
    ~args:(fun () -> [ nat 3; nat 4 ])
    (H.returns (fun () -> [ V.text "large" ]))
