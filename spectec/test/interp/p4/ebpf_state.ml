open Common.Annotated
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

(* A callback that answers [$find_var_e] for the named arguments of a call. *)
let callback_of_arguments arguments : Builtins.Callback.t =
 fun id _targs values ->
  match (id.it, values) with
  | "find_var_e", { it = CaseV notation; _ } :: _ -> (
      match Mixfix.args notation with
      | [ { it = TextV name; _ } ] -> List.assoc name arguments
      | _ -> failwith "unexpected variable name")
  | _ -> failwith ("unexpected callback to " ^ id.it)

let check_counter_updates () =
  let ctx = Value.text "ctx" in
  (match
     Ebpf.increment
       ~callback:(callback_of_arguments [ ("index", bit32 2) ])
       ~at:Location.Unknown ~ctx [ 0; 0; 0; 0 ]
   with
  | Ok ([ 0; 0; 1; 0 ], _, _) -> ()
  | _ -> failwith "increment did not update the indexed counter");
  match
    Ebpf.add
      ~callback:
        (callback_of_arguments [ ("index", bit32 1); ("value", bit32 5) ])
      ~at:Location.Unknown ~ctx [ 0; 0; 0; 0 ]
  with
  | Ok ([ 0; 5; 0; 0 ], _, _) -> ()
  | _ -> failwith "add did not update the indexed counter"

let () =
  List.iter
    (fun size -> List.iter (check_initial_state size) [ false; true ])
    [ 0; 1; 4 ];
  check_counter_updates ()
