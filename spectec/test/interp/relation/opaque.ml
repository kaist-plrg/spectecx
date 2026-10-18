module H = Harness
module V = Lang.Il.Value
module B = Interp.Builtins
module O = Lang.Il.Opaque

let ( $ ) = Common.Source.( $ )

let typdef_state =
  O.make_typdef ~name:"state" ~compare:Int.compare ~hash:Hashtbl.hash

let typdef_other =
  O.make_typdef ~name:"otherState" ~compare:String.compare ~hash:Hashtbl.hash

let typdef_conflicting =
  O.make_typdef ~name:"state" ~compare:String.compare ~hash:Hashtbl.hash

let state n = V.opaque typdef_state n
let nat n = V.nat (Bigint.of_int n)

let box saved =
  let typ = Lang.Il.Typ.var "state" [] $ Common.Source.no_region in
  let field name value =
    (Lang.Xl.Atom.keyword name $ Common.Source.no_region, value)
  in
  V.record "box"
    [
      field "CURRENT" (state 0);
      field "HISTORY" (V.list typ [ state 0 ]);
      field "SAVED" (V.opt typ (Some saved));
    ]

module Target : Interp.Target.S = struct
  let opaque_types = [ O.Typdef typdef_state; O.Typdef typdef_other ]
  let state_version = ref 0
  let is_impure_func _ = false
  let is_impure_rel _ = false

  let with_state f =
    let next_vid = ref 0 in
    V.GlobalVidProvider.with_provider
      (fun () ->
        let vid = !next_vid in
        incr next_vid;
        vid)
      f

  let builtins =
    [
      ( "make_state",
        B.Define.T0.a1 B.Arg.nat (fun ~at:_ n ->
            Ok (state (Bigint.to_int_exn n))) );
      ( "read_state",
        B.Define.T0.a1 (B.Arg.opaque typdef_state) (fun ~at:_ count ->
            Ok (nat count)) );
      ( "increment_state",
        B.Define.T0.a1 (B.Arg.opaque typdef_state) (fun ~at:_ count ->
            Ok (state (count + 1))) );
      ( "wrong_state",
        B.Define.T0.a0 (fun ~at:_ -> Ok (V.opaque typdef_conflicting "wrong"))
      );
      ( "wrong_box",
        B.Define.T0.a0 (fun ~at:_ ->
            let typ = Lang.Il.Typ.var "state" [] $ Common.Source.no_region in
            Ok
              (V.tuple
                 [ nat 0; V.list typ [ V.opaque typdef_conflicting "wrong" ] ]))
      );
      ( "wrong_record",
        B.Define.T0.a0 (fun ~at:_ ->
            Ok (box (V.opaque typdef_conflicting "wrong"))) );
      ( "identity_state",
        B.Define.T1.a1 B.Arg.value (fun ~at:_ _ value -> Ok value) );
    ]
end

let typdef_text_state =
  O.make_typdef ~name:"state" ~compare:String.compare ~hash:Hashtbl.hash

module Text_target : Interp.Target.S = struct
  include Target

  let opaque_types = [ O.Typdef typdef_text_state; O.Typdef typdef_other ]

  let builtins =
    [
      ( "identity_state",
        B.Define.T1.a1 B.Arg.value (fun ~at:_ _ value -> Ok value) );
    ]
end

let spec = H.compile_file "opaque.spectec"

let check_source_roundtrip () =
  let parse contents =
    Spectec.parse_spec_source Spectec.{ filename = "opaque.spectec"; contents }
    |> Result.get_ok
  in
  let source = Core.In_channel.read_all "opaque.spectec" |> parse in
  let printed = Lang.El.Unparse.string_of_spec source in
  if not (Lang.El.Eq.eq_spec source (parse printed)) then
    failwith "opaque declaration source roundtrip"

let run () =
  check_source_roundtrip ();
  H.check spec
    ~target:(module Target)
    ~name:"opaque values pass through aliases, functions, and composite values"
    ~relation:"Roundtrip"
    ~args:(fun () -> [ nat 3 ])
    (H.returns (fun () ->
         let typ = Lang.Il.Typ.var "state" [] $ Common.Source.no_region in
         [ nat 4; V.list typ [ state 4 ]; V.opt typ (Some (state 4)) ]));
  H.check spec
    ~target:(module Target)
    ~name:"opaque membership accepts its declared type" ~relation:"Membership"
    ~args:(fun () -> [ state 3 ])
    (H.returns (fun () -> [ V.bool true ]));
  H.check spec
    ~target:(module Target)
    ~name:"opaque host inputs reject another opaque type" ~relation:"Membership"
    ~args:(fun () -> [ V.opaque typdef_other "key" ])
    (H.fails_with
       "does not match the registered opaque type definition for state");
  H.check spec
    ~target:(module Target)
    ~name:"opaque host inputs reject ordinary values" ~relation:"Membership"
    ~args:(fun () -> [ nat 3 ])
    (H.fails_with "expected state");
  H.check spec
    ~target:(module Target)
    ~name:"equal opaque values in expressions" ~relation:"Equal"
    ~args:(fun () -> [ nat 3; nat 3 ])
    (H.returns (fun () -> [ V.bool true ]));
  H.check spec
    ~target:(module Target)
    ~name:"distinct opaque values in expressions" ~relation:"Equal"
    ~args:(fun () -> [ nat 3; nat 4 ])
    (H.returns (fun () -> [ V.bool false ]));
  H.check spec
    ~target:(module Target)
    ~name:"input type definition identity is stronger than its name"
    ~relation:"Membership"
    ~args:(fun () -> [ V.opaque typdef_conflicting "wrong" ])
    (H.fails_with
       "does not match the registered opaque type definition for state");
  H.check spec
    ~target:(module Target)
    ~name:"builtin results require the registered type definition"
    ~relation:"Wrong_result"
    ~args:(fun () -> [ nat 0 ])
    (H.fails_with "builtin $wrong_state result");
  H.check spec
    ~target:(module Target)
    ~name:"nested builtin results require the registered type definition"
    ~relation:"Wrong_nested_result"
    ~args:(fun () -> [ nat 0 ])
    (H.fails_with "builtin $wrong_box result");
  H.check spec
    ~target:(module Target)
    ~name:"builtin record results validate optional opaque fields"
    ~relation:"Wrong_record_result"
    ~args:(fun () -> [ nat 0 ])
    (H.fails_with "builtin $wrong_record result");
  H.check spec
    ~target:(module Target)
    ~name:"task input records validate optional opaque fields"
    ~relation:"Read_box"
    ~args:(fun () -> [ box (V.opaque typdef_conflicting "wrong") ])
    (H.fails_with
       "does not match the registered opaque type definition for state");
  H.check spec
    ~target:(module Target)
    ~name:"polymorphic builtin results resolve the expected opaque type"
    ~relation:"Builtin_identity"
    ~args:(fun () -> [ state 3 ])
    (H.returns (fun () -> [ state 3 ]));
  H.check spec
    ~target:(module Text_target)
    ~name:"a second target provides its own native representation"
    ~relation:"Builtin_identity"
    ~args:(fun () -> [ V.opaque typdef_text_state "native text" ])
    (H.returns (fun () -> [ V.opaque typdef_text_state "native text" ]));
  let module Missing = struct
    include Target

    let opaque_types = []
  end in
  H.check spec
    ~target:(module Missing)
    ~name:"opaque declarations require host bindings" ~relation:"Membership"
    ~args:(fun () -> [ state 3 ])
    (H.fails_with "opaque type state has no host binding");
  let module Duplicate = struct
    include Target

    let opaque_types = O.Typdef typdef_conflicting :: opaque_types
  end in
  H.check spec
    ~target:(module Duplicate)
    ~name:"opaque declarations reject duplicate host bindings"
    ~relation:"Membership"
    ~args:(fun () -> [ state 3 ])
    (H.fails_with "opaque type state has multiple host bindings")
