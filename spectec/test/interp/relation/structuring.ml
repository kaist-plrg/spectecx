module H = Harness
module V = Lang.Il.Value

let spec = H.compile_file "structuring.spectec"
let nat n = V.nat (Bigint.of_int n)

let check_relation_calls ~relation ~expected =
  let def =
    List.find
      (fun (def : Lang.Sl.def) ->
        match def.it with
        | Lang.Sl.RelD (id, _, _, _) -> id.it = relation
        | _ -> false)
      spec.spec_sl
  in
  let calls =
    Core.String.substr_index_all
      (Lang.Sl.Print.string_of_def def)
      ~pattern:"(Double:" ~may_overlap:false
    |> List.length
  in
  if calls <> expected then
    failwith
      (Printf.sprintf "%s: expected %d relation calls, got %d" relation expected
         calls)

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
    H.fails;
  H.check spec ~name:"different relation inputs reject unmatched rules"
    ~relation:"Pick"
    ~args:(fun () -> [ nat 7; nat 3 ])
    H.fails;
  H.check spec ~name:"different relation inputs select the first rule"
    ~relation:"Pick"
    ~args:(fun () -> [ nat 3; nat 7 ])
    (H.returns (fun () -> [ nat 6 ]));
  H.check spec ~name:"different relation inputs select the second rule"
    ~relation:"Pick"
    ~args:(fun () -> [ nat 7; nat 8 ])
    (H.returns (fun () -> [ nat 16 ]));
  List.iter
    (fun (a, b, expected) ->
      H.check spec
        ~name:("shared relation input returns " ^ expected)
        ~relation:"Size"
        ~args:(fun () -> [ nat a; nat b ])
        (H.returns (fun () -> [ V.text expected ])))
    [ (1, 2, "small"); (3, 4, "large") ];
  check_relation_calls ~relation:"Size" ~expected:1;
  check_relation_calls ~relation:"Pick" ~expected:2
