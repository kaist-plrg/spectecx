open Lang.Il
open Builtins

let ( let* ) = Result.bind

type object_state = CounterArray of int list

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

let builtins =
  [
    ( "init_objectState",
      Define.T0.a4 Arg.text (Arg.list_of Arg.value) (Arg.list_of Arg.text)
        (Arg.list_of Arg.value) init_object_state );
    ( "init_archState",
      Define.T0.a0 (fun ~at:_ -> Ok (Value.opaque typdef_arch_state ())) );
  ]
