open Common.Source
open Lang.Il
open Builtins
open Mixfix

let ( let* ) = Result.bind

type object_state = CounterArray of int list

let typdef_object_state =
  Opaque.make_typdef ~name:"objectState" ~compare:Stdlib.compare
    ~hash:Hashtbl.hash

let opaque_types = [ Opaque.Typdef typdef_object_state ]

let bit32_value at value =
  match value.it with
  | CaseV [ Arg width; Atom atom; Arg bits ]
    when Lang.Xl.Atom.eq atom.it (Lang.Xl.Atom.keyword "W") ->
      let* width = Arg.nat at width in
      let* bits = Arg.num at bits in
      let bits = Lang.Xl.Num.to_int bits in
      if Bigint.(width = of_int 32 && bits >= zero && bits < one lsl 32) then
        Ok bits
      else Error (Error.type_err at "bit<32> value" value)
  | _ -> Error (Error.type_err at "bit<32> value" value)

let bool_value at value =
  match value.it with
  | CaseV [ Atom atom; Arg value ]
    when Lang.Xl.Atom.eq atom.it (Lang.Xl.Atom.tag "B") ->
      Arg.bool at value
  | _ -> Error (Error.type_err at "P4 boolean value" value)

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
        let* size = bit32_value at size in
        let* sparse = find "sparse" in
        let* _sparse = bool_value at sparse in
        let counters = List.init (Bigint.to_int_exn size) (fun _ -> 0) in
        Ok (Value.opaque typdef_object_state (CounterArray counters))
    | "CounterArray", _ ->
        Error (Error.runtime at "CounterArray does not take type arguments")
    | _ -> Error (Error.runtime at ("unsupported eBPF extern object: " ^ name))

let init_object_state_builtin ~at targs args =
  match (targs, args) with
  | [], [ name; type_args; ids; values ] ->
      let* name = Arg.text at name in
      let* type_args = Arg.list_of Arg.value at type_args in
      let* ids = Arg.list_of Arg.text at ids in
      let* values = Arg.list_of Arg.value at values in
      init_object_state ~at name type_args ids values
  | [], _ -> Error (Error.arity at "init_objectState expects four arguments")
  | _ -> Error (Error.arity at "init_objectState does not take type arguments")

let builtins = [ ("init_objectState", init_object_state_builtin) ]
