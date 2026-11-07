open Lang.Il
open Builtins

let ( let* ) = Result.bind

type object_state =
  | CounterArray of int list
  | PacketIn of Packet_in.t
  (* [array_table] and [hash_table] only configure a table and keep no state. *)
  | TableImplementation

let typdef_object_state =
  Opaque.make_typdef ~name:"objectState" ~compare:Stdlib.compare
    ~hash:Hashtbl.hash

(* The eBPF architecture keeps no state of its own. *)
let typdef_arch_state =
  Opaque.make_typdef ~name:"archState" ~compare:Stdlib.compare
    ~hash:Hashtbl.hash

let opaque_types =
  [ Opaque.Typdef typdef_object_state; Opaque.Typdef typdef_arch_state ]

let init_object_state ~at name type_args ids values =
  if List.length ids <> List.length values then
    Error
      (Error.arity at "extern constructor parameter and argument counts differ")
  else
    match (name, type_args) with
    | ("array_table" | "hash_table"), [] ->
        Ok (Value.opaque typdef_object_state TableImplementation)
    | "packet_in", [] ->
        Ok (Value.opaque typdef_object_state (PacketIn (Packet_in.make [||])))
    | "CounterArray", [] ->
        let args = List.combine ids values in
        let find name =
          match List.assoc_opt name args with
          | Some value -> Ok value
          | None -> Error (Error.runtime at ("CounterArray is missing " ^ name))
        in
        let* size = find "max_index" in
        let* size = P4_values.bit32_of_value at size in
        let* sparse = find "sparse" in
        let* _sparse = P4_values.bool_of_value at sparse in
        let counters = List.init (Bigint.to_int_exn size) (fun _ -> 0) in
        Ok (Value.opaque typdef_object_state (CounterArray counters))
    | "CounterArray", _ ->
        Error (Error.runtime at "CounterArray does not take type arguments")
    | _ -> Error (Error.runtime at ("unsupported eBPF extern object: " ^ name))

let packet_in_state bits =
  Value.opaque typdef_object_state (PacketIn (Packet_in.make bits))

(* Counter arrays *)

let find_bit32 ~callback ~at ctx ~name =
  let* n =
    Spec_calls.find_var_local_exn ~callback ctx ~name
    |> P4_values.bit32_of_value at
  in
  Ok (Bigint.to_int_exn n)

let add_to_counter counters ~index ~value =
  List.mapi
    (fun idx count -> if idx = index then count + value else count)
    counters

(* [void increment(in bit<32> index)] *)

let increment ~callback ~at ~ctx counters =
  let* index = find_bit32 ~callback ~at ctx ~name:"index" in
  Ok (add_to_counter counters ~index ~value:1, ctx, P4_values.return_empty)

(* [void add(in bit<32> index, in bit<32> value)] *)

let add ~callback ~at ~ctx counters =
  let* index = find_bit32 ~callback ~at ctx ~name:"index" in
  let* value = find_bit32 ~callback ~at ctx ~name:"value" in
  Ok (add_to_counter counters ~index ~value, ctx, P4_values.return_empty)

(* Extern calls *)

let unsupported ~at ~kind ~name ~params =
  Error
    (Error.runtime at
       (Printf.sprintf "unsupported eBPF extern %s call: %s(%s)" kind name
          (String.concat ", " params)))

(* [extern void verify(in bool check, in error toSignal)] *)

let verify ~callback ~at ctx =
  let* check =
    Spec_calls.find_var_local_exn ~callback ctx ~name:"check"
    |> P4_values.bool_of_value at
  in
  if check then Ok P4_values.return_empty
  else
    Ok
      (P4_values.reject
         (Spec_calls.find_var_local_exn ~callback ctx ~name:"toSignal"))

let missing_object ~at object_id =
  Error.runtime at ("unknown extern object " ^ Value.to_string object_id)

let eval_extern_function_call ~callback ~at ctx arch name params =
  let* call_result =
    match (name, params) with
    | "verify", [ "check"; "toSignal" ] -> verify ~callback ~at ctx
    | _ -> unsupported ~at ~kind:"function" ~name ~params
  in
  Ok (Value.tuple [ ctx; arch; call_result ])

let eval_extern_method_call ~callback ~at ctx arch object_id name params =
  let* object_state =
    Spec_calls.find_object_state_opt ~callback ~at arch ~object_id
  in
  let* object_state =
    Option.to_result ~none:(missing_object ~at object_id) object_state
  in
  let* object_state = Arg.opaque typdef_object_state at object_state in
  let* object_state, ctx, call_result =
    match (object_state, name, params) with
    | PacketIn packet, "extract", [ "hdr" ] ->
        let* packet, ctx, call_result =
          Packet_in.extract ~callback ~at ~ctx packet
        in
        Ok (PacketIn packet, ctx, call_result)
    | CounterArray counters, "increment", [ "index" ] ->
        let* counters, ctx, call_result =
          increment ~callback ~at ~ctx counters
        in
        Ok (CounterArray counters, ctx, call_result)
    | CounterArray counters, "add", [ "index"; "value" ] ->
        let* counters, ctx, call_result = add ~callback ~at ~ctx counters in
        Ok (CounterArray counters, ctx, call_result)
    | _ -> unsupported ~at ~kind:"method" ~name ~params
  in
  let state = Value.opaque typdef_object_state object_state in
  let* arch =
    Spec_calls.update_object_state_opt ~callback ~at arch ~object_id ~state
  in
  let* arch = Option.to_result ~none:(missing_object ~at object_id) arch in
  Ok (Value.tuple [ ctx; arch; call_result ])

let builtins =
  [
    ( "init_objectState",
      Define.T0.a4 Arg.text (Arg.list_of Arg.value) (Arg.list_of Arg.text)
        (Arg.list_of Arg.value) init_object_state );
    ( "init_archState",
      Define.T0.a0 (fun ~at:_ -> Ok (Value.opaque typdef_arch_state ())) );
    ( "eval_extern_function_call",
      Define.make_with_callback
        ~args:(Define.T0.a4 Arg.value Arg.value Arg.text (Arg.list_of Arg.text))
        eval_extern_function_call );
    ( "eval_extern_method_call",
      Define.make_with_callback
        ~args:
          (Define.T0.a5 Arg.value Arg.value Arg.value Arg.text
             (Arg.list_of Arg.text))
        eval_extern_method_call );
  ]
