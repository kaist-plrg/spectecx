module H = Harness
module V = Lang.Il.Value

let spec = H.compile_file "structuring.spectec"

let run () =
  List.iter
    (fun (n, expected) ->
      H.check spec
        ~name:("merged case returns " ^ expected)
        ~relation:"Describe"
        ~args:(fun () -> [ V.nat (Bigint.of_int n) ])
        (H.returns (fun () -> [ V.text expected ])))
    [ (2, "even"); (3, "small odd"); (11, "large odd") ];
  H.check spec ~name:"shared predicate rejects zero" ~relation:"Describe"
    ~args:(fun () -> [ V.nat Bigint.zero ])
    H.fails
