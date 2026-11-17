module H = Harness
module V = Lang.Il.Value

let spec = H.compile_file "structuring.spectec"
let nat n = V.nat (Bigint.of_int n)
let check_text_results = H.check_text_results spec

let run () =
  check_text_results ~relation:"Describe"
    [
      ("2", (fun () -> [ nat 2 ]), "even");
      ("3", (fun () -> [ nat 3 ]), "small odd");
      ("11", (fun () -> [ nat 11 ]), "large odd");
    ];
  H.check spec ~name:"shared predicate rejects zero" ~relation:"Describe"
    ~args:(fun () -> [ nat 0 ])
    H.fails
