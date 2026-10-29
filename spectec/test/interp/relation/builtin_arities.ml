module H = Harness
module V = Lang.Il.Value
module B = Interp.Builtins

let nat n = V.nat (Bigint.of_int n)

let nats ns =
  V.list Common.Source.(Lang.Il.Typ.nat $ no_region) (List.map nat ns)

module Target : Interp.Target.S = struct
  include H.Pure_target

  let digits ~at:_ d1 d2 d3 d4 d5 =
    let ten = Bigint.of_int 10 in
    Ok
      (V.nat
         (List.fold_left
            (fun number digit -> Bigint.((number * ten) + digit))
            Bigint.zero [ d1; d2; d3; d4; d5 ]))

  let builtins =
    [
      ( "digits",
        B.Define.T0.a5 B.Arg.nat B.Arg.nat B.Arg.nat B.Arg.nat B.Arg.nat digits
      );
      ( "reverse",
        B.Define.T1.a3 B.Arg.value B.Arg.value B.Arg.value
          (fun ~at:_ typ v1 v2 v3 -> Ok (V.list typ [ v3; v2; v1 ])) );
    ]
end

let spec = H.compile_file "builtin_arities.spectec"

let run () =
  H.check spec
    ~target:(module Target)
    ~name:"five builtin arguments arrive in order" ~relation:"Digits"
    ~args:(fun () -> [ nat 1; nat 2; nat 3; nat 4; nat 5 ])
    (H.returns (fun () -> [ nat 12345 ]));
  H.check spec
    ~target:(module Target)
    ~name:"three builtin arguments after a type argument arrive in order"
    ~relation:"Reverse"
    ~args:(fun () -> [ nat 1; nat 2; nat 3 ])
    (H.returns (fun () -> [ nats [ 3; 2; 1 ] ]))
