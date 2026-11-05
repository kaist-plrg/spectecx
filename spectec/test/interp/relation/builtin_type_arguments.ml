module H = Harness
module V = Lang.Il.Value
module B = Interp.Builtins

module Target : Interp.Target.S = struct
  include H.Pure_target

  let builtins =
    [
      ( "type_name",
        B.Define.T1.a1 B.Arg.value (fun ~at:_ typ _ ->
            Ok (V.text (Lang.Il.Print.string_of_typ typ))) );
    ]
end

let spec = H.compile_file "builtin_type_arguments.spectec"

let run () =
  H.check spec
    ~target:(module Target)
    ~name:"a builtin called from a generic function receives the caller's type"
    ~relation:"TypeName"
    ~args:(fun () -> [ V.nat (Bigint.of_int 1) ])
    (H.returns (fun () -> [ V.text "nat" ]))
