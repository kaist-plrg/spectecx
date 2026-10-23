module P4 = Targets_p4.P4
module Ebpf = P4.Builtins.Ebpf
module V = Lang.Il.Value
module B = Interp.Builtins

let ( $ ) = Common.Source.( $ )
let at = Common.Source.no_region

let unwrap = function
  | Ok value -> value
  | Error error ->
      prerr_endline
        (Spectec.Error.to_diagnostics error
        |> Spectec.Diagnostic.Render.render_bag
             ~ansi:Spectec.Diagnostic.Ansi.plain);
      exit 1

let compile () =
  let spec =
    Spectec.collect_spec_files "../../../specs/p4"
    |> Spectec.parse_spec_files |> unwrap
  in
  let spec_il = Spectec.elaborate spec |> unwrap in
  let henv = Spectec.henv_with_il_spec (Spectec.henv_of_el_spec spec) spec_il in
  (spec_il, henv)

let expected_state size =
  let open Lang.Il in
  let value_type = Typ.var "value" [] in
  let size =
    V.Make.case value_type
      [
        Mixfix.Arg (V.nat (Bigint.of_int 32));
        Mixfix.Atom (Lang.Xl.Atom.keyword "W" $ at);
        Mixfix.Arg (V.int (Bigint.of_int size));
      ]
  in
  let sparse =
    V.Make.case value_type
      [ Mixfix.Atom (Lang.Xl.Atom.tag "B" $ at); Mixfix.Arg (V.bool false) ]
  in
  let builtins = B.make Ebpf.builtins in
  match
    builtins.invoke ("init_objectState" $ at) []
      [
        V.text "CounterArray";
        V.list (Typ.var "typeIR" [] $ at) [];
        V.list (Typ.text $ at) [ V.text "max_index"; V.text "sparse" ];
        V.list (value_type $ at) [ size; sparse ];
      ]
  with
  | Ok state -> state
  | Error error -> failwith (snd (B.Error.region_and_message error))

let object_state_opt (object_value : V.t) =
  match object_value.it with
  | Lang.Il.CaseV notation -> (
      match Lang.Il.Mixfix.args notation with
      | [
       _name; _types; ({ it = Lang.Il.OpaqueV _; _ } as state); _frame; _methods;
      ] ->
          Some state
      | _ -> None)
  | _ -> None

let check_store mode expected = function
  | [ _global; store ] ->
      let objects =
        B.Arg.map Common.Source.no_region store
        |> Result.get_ok |> B.Arg.VMap.bindings
      in
      if List.length objects <> List.length expected then
        failwith (mode ^ ": unexpected number of objects");
      List.iter
        (fun (name, size) ->
          let object_value =
            List.find
              (fun (id, _) -> V.get_list id |> List.map V.get_text = [ name ])
              objects
            |> snd
          in
          match object_state_opt object_value with
          | Some state when V.eq state (expected_state size) -> ()
          | _ -> failwith (mode ^ ": incorrect initial state for " ^ name))
        expected
  | _ -> failwith (mode ^ ": expected global context and store")

let run spec_il mode filename =
  let input : P4.Instantiate.input =
    { includes = []; filename; expect = Spectec.Task.Positive }
  in
  Spectec.eval_task (module P4.Instantiate) ~mode ~spec_il input

let check_nested_store mode = function
  | [ _global; store ] ->
      let objects =
        B.Arg.map Common.Source.no_region store
        |> Result.get_ok |> B.Arg.VMap.bindings
      in
      let states =
        List.filter_map (fun (_, value) -> object_state_opt value) objects
        |> List.sort V.compare
      in
      let expected = List.map expected_state [ 5; 6 ] |> List.sort V.compare in
      if List.length objects <> 5 || not (List.equal V.eq states expected) then
        failwith (mode ^ ": nested constructor state was lost")
  | _ -> failwith (mode ^ ": expected global context and store")

let () =
  let spec_il, henv = compile () in
  List.iter
    (fun (name, mode) ->
      run spec_il mode "counter-array.p4"
      |> unwrap
      |> check_store name [ ("positional", 3); ("named", 4); ("empty", 0) ];
      run spec_il mode "counter-default.p4"
      |> unwrap
      |> check_store name
           [ ("defaults", 2); ("named_defaults", 1); ("all_defaults", 2) ];
      run spec_il mode "counter-nested.p4" |> unwrap |> check_nested_store name;
      match run spec_il mode "unsupported.p4" with
      | Ok _ -> failwith (name ^ ": unsupported object was instantiated")
      | Error error ->
          let diagnostics =
            Spectec.Error.to_diagnostics error
            |> Spectec.Diagnostic.Render.render_bag
                 ~ansi:Spectec.Diagnostic.Ansi.plain
          in
          if
            not
              (P4.contains_substring diagnostics
                 "unsupported eBPF extern object: Unsupported")
          then failwith diagnostics)
    [ ("IL", Spectec.Interp_mode.Il); ("SL", Sl); ("PL", Pl henv) ]
