module V = Lang.Il.Value
module O = Lang.Il.Opaque
module Arg = Interp.Builtins.Arg

type packet_state = { data : string; cursor : int }

let typdef_packet =
  O.make_typdef ~name:"packetState" ~compare:Stdlib.compare ~hash:Hashtbl.hash

let typdef_key =
  O.make_typdef ~name:"verificationKey" ~compare:String.compare
    ~hash:Hashtbl.hash

let typdef_conflicting =
  O.make_typdef ~name:"packetState" ~compare:String.compare ~hash:Hashtbl.hash

let require name condition = if not condition then failwith name

let run () =
  let payload = { data = "packet"; cursor = 3 } in
  let stored = O.of_ocaml ~typdef:typdef_packet payload in
  let typdef_same_name =
    O.make_typdef ~name:"packetState" ~compare:Stdlib.compare ~hash:Hashtbl.hash
  in
  require "opaque type check accepts the original type definition"
    (O.has_type stored ~typdef:typdef_packet);
  require "opaque type check rejects a different representation"
    (not (O.has_type stored ~typdef:typdef_conflicting));
  require "fresh type definitions remain distinct with the same representation"
    (O.to_ocaml_opt ~typdef:typdef_same_name stored = None
    && (not (O.has_type stored ~typdef:typdef_same_name))
    && O.compare stored (O.of_ocaml ~typdef:typdef_same_name payload) <> 0);
  let state = V.opaque typdef_packet payload in
  let equal_state = V.opaque typdef_packet { data = "packet"; cursor = 3 } in
  let updated_state = V.opaque typdef_packet { payload with cursor = 4 } in
  let key = V.opaque typdef_key "verification key" in
  let incompatible = V.opaque typdef_conflicting "different representation" in
  require "opaque equality" (V.eq state equal_state);
  require "opaque AST equality" (Lang.Il.Eq.eq_value state equal_state);
  require "equal opaque values have equal hashes"
    (state.note.vhash = equal_state.note.vhash);
  require "opaque payload affects equality" (not (V.eq state updated_state));
  require "opaque type definition affects equality" (not (V.eq state key));
  require "matching names do not establish host type identity"
    (not (V.eq state incompatible));
  require "opaque AST equality checks type definition identity"
    (not (Lang.Il.Eq.eq_value state incompatible));
  require "opaque comparison is antisymmetric across representations"
    (Int.compare (V.compare state incompatible) 0
    = -Int.compare (V.compare incompatible state) 0);
  require "opaque composite equality"
    (V.eq (V.tuple [ state ]) (V.tuple [ equal_state ]));
  let states = Arg.VSet.of_list [ state; equal_state; updated_state; key ] in
  require "opaque set keys" (Arg.VSet.cardinal states = 3);
  require "native packet state extraction"
    (Arg.opaque typdef_packet Common.Source.no_region state = Ok payload);
  require "native verification key extraction"
    (Arg.opaque typdef_key Common.Source.no_region key = Ok "verification key");
  require "opaque extraction preserves the native payload"
    (match Arg.opaque typdef_packet Common.Source.no_region state with
    | Ok actual -> actual == payload
    | Error _ -> false);
  require "opaque extraction rejects another type definition with the same name"
    (Result.is_error
       (Arg.opaque typdef_packet Common.Source.no_region incompatible));
  require "opaque extraction rejects ordinary values"
    (Result.is_error
       (Arg.opaque typdef_packet Common.Source.no_region (V.text "packet")));
  require "opaque printing hides the payload"
    (V.to_string state = "<opaque packetState>")
