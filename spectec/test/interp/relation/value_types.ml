module H = Harness
module V = Lang.Il.Value
module B = Interp.Builtins
module O = Lang.Il.Opaque

let ( $ ) = Common.Source.( $ )
let nat n = V.nat (Bigint.of_int n)

let field name value =
  (Lang.Xl.Atom.keyword name $ Common.Source.no_region, value)

let box saved =
  let typ = Lang.Il.Typ.var "number" [] $ Common.Source.no_region in
  V.record "box"
    [
      field "CURRENT" (nat 3);
      field "HISTORY" (V.list typ [ nat 1; nat 2 ]);
      field "SAVED" (V.opt typ (Some saved));
    ]

let reversed_pair () =
  V.record "pair" [ field "RIGHT" (nat 2); field "LEFT" (nat 1) ]

module Target : Interp.Target.S = struct
  include H.Pure_target

  let builtins =
    [
      ("identity", B.Define.T1.a1 B.Arg.value (fun ~at:_ _ value -> Ok value));
      ( "wrong_identity",
        B.Define.T1.a1 B.Arg.value (fun ~at:_ _ _ -> Ok (V.text "wrong")) );
      ("wrong_box", B.Define.T0.a0 (fun ~at:_ -> Ok (box (V.text "wrong"))));
      ("reversed_pair", B.Define.T0.a0 (fun ~at:_ -> Ok (reversed_pair ())));
      ("negative_nat", B.Define.T0.a0 (fun ~at:_ -> Ok (nat (-1))));
    ]
end

let typdef_unused =
  O.make_typdef ~name:"unused" ~compare:Int.compare ~hash:Hashtbl.hash

module With_opaque : Interp.Target.S = struct
  include Target

  let opaque_types = [ O.Typdef typdef_unused ]
end

let spec = H.compile_file "value_types.spectec"

let check_target label target =
  let check ~name = H.check spec ~target ~name:(label ^ ": " ^ name) in
  check ~name:"valid host input resolves aliases" ~relation:"Read_box"
    ~args:(fun () -> [ box (nat 4) ])
    (H.returns (fun () -> [ nat 3 ]));
  check ~name:"host input validates nested ordinary values" ~relation:"Read_box"
    ~args:(fun () -> [ box (V.text "wrong") ])
    (H.fails_with "expected nat");
  check ~name:"host input validates arity" ~relation:"Read_box"
    ~args:(fun () -> [])
    (H.fails_with "relation Read_box input: arity mismatch: expected 1, got 0");
  check ~name:"host input rejects extra arguments" ~relation:"Read_box"
    ~args:(fun () -> [ box (nat 4); nat 0 ])
    (H.fails_with "relation Read_box input: arity mismatch: expected 1, got 2");
  check ~name:"polymorphic builtin result resolves aliases"
    ~relation:"Builtin_identity"
    ~args:(fun () -> [ box (nat 4) ])
    (H.returns (fun () -> [ box (nat 4) ]));
  check ~name:"builtin result validates nested ordinary values"
    ~relation:"Wrong_result"
    ~args:(fun () -> [ nat 0 ])
    (H.fails_with "builtin $wrong_box result");
  check ~name:"builtin result instantiates the declared type parameter"
    ~relation:"Wrong_polymorphic_result"
    ~args:(fun () -> [ nat 3 ])
    (H.fails_with "builtin $wrong_identity result");
  check ~name:"record destructuring follows declaration order"
    ~relation:"Read_pair"
    ~args:(fun () ->
      [ V.record "pair" [ field "LEFT" (nat 1); field "RIGHT" (nat 2) ] ])
    (H.returns (fun () -> [ nat 1 ]));
  check ~name:"host input rejects reversed record fields" ~relation:"Read_pair"
    ~args:(fun () -> [ reversed_pair () ])
    (H.fails_with "relation Read_pair input: expected pair");
  check ~name:"builtin result rejects reversed record fields"
    ~relation:"Reversed_result"
    ~args:(fun () -> [ nat 0 ])
    (H.fails_with "builtin $reversed_pair result: expected pair");
  check ~name:"host input accepts zero as a natural"
    ~relation:"Natural_identity"
    ~args:(fun () -> [ nat 0 ])
    (H.returns (fun () -> [ nat 0 ]));
  check ~name:"host input rejects a negative natural"
    ~relation:"Natural_identity"
    ~args:(fun () -> [ nat (-1) ])
    (H.fails_with "relation Natural_identity input: expected nat");
  check ~name:"host input rejects a negative integer as a natural"
    ~relation:"Natural_identity"
    ~args:(fun () -> [ V.int (Bigint.of_int (-1)) ])
    (H.fails_with "relation Natural_identity input: expected nat");
  check ~name:"builtin result rejects a negative natural"
    ~relation:"Negative_result"
    ~args:(fun () -> [ nat 0 ])
    (H.fails_with "builtin $negative_nat result: expected nat")

let run () =
  check_target "no opaque types" (module Target);
  check_target "unused opaque type" (module With_opaque);
  let typdef_duplicate =
    O.make_typdef ~name:"unused" ~compare:String.compare ~hash:Hashtbl.hash
  in
  let module Duplicate = struct
    include With_opaque

    let opaque_types = O.Typdef typdef_duplicate :: opaque_types
  end in
  H.check spec
    ~target:(module Duplicate)
    ~name:"duplicate opaque registrations are rejected even when unused"
    ~relation:"Read_box"
    ~args:(fun () -> [ box (nat 4) ])
    (H.fails_with "opaque type unused has multiple host bindings")
