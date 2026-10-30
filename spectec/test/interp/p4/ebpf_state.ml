open Common.Source
module Location = Common.Location
open Lang.Il

let bit32 size =
  Value.Make.case (Typ.var "value" [])
    [
      Mixfix.Arg (Value.nat (Bigint.of_int 32));
      Mixfix.Atom (Lang.Xl.Atom.keyword "W" $ Location.Unknown);
      Mixfix.Arg (Value.int (Bigint.of_int size));
    ]

let p4_bool value =
  Value.Make.case (Typ.var "value" [])
    [
      Mixfix.Atom (Lang.Xl.Atom.tag "B" $ Location.Unknown);
      Mixfix.Arg (Value.bool value);
    ]

let check_initial_state size sparse =
  match
    Ebpf.init_object_state ~at:Location.Unknown "CounterArray" []
      [ "sparse"; "max_index" ]
      [ p4_bool sparse; bit32 size ]
  with
  | Error error -> failwith (snd (Builtins.Error.location_and_message error))
  | Ok state -> (
      match
        Builtins.Arg.opaque Ebpf.typdef_object_state Location.Unknown state
      with
      | Ok (Ebpf.CounterArray counters)
        when counters = List.init size (fun _ -> 0) ->
          ()
      | _ -> failwith "incorrect initial counter array contents")

let () =
  List.iter
    (fun size -> List.iter (check_initial_state size) [ false; true ])
    [ 0; 1; 4 ]
