module P4 = Targets_p4.P4
module Ebpf = P4.Builtins.Ebpf
module V = Lang.Il.Value
module B = Interp.Builtins

let ( $ ) = Common.Annotated.( $ )
let at = Common.Location.Unknown

let render error =
  Spectec.Error.to_diagnostics error
  |> Spectec.Diagnostic.Render.render_bag ~ansi:Spectec.Diagnostic.Ansi.plain

let unwrap = function
  | Ok value -> value
  | Error error ->
      prerr_endline (render error);
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
    builtins.invoke
      ~callback:(fun _ _ _ -> failwith "init_objectState does not call back")
      ("init_objectState" $ at) []
      [
        V.text "CounterArray";
        V.list (Typ.var "typeIR" [] $ at) [];
        V.list (Typ.text $ at) [ V.text "max_index"; V.text "sparse" ];
        V.list (value_type $ at) [ size; sparse ];
      ]
  with
  | Ok state -> state
  | Error error -> failwith (snd (B.Error.location_and_message error))

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
        B.Arg.map Common.Location.Unknown store
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

let samples = "../../../testdata/interp/p4/p4c/p4_16_samples/"

let simulate spec_il mode filename ~stf =
  let input : P4.Sim.input =
    { includes = [ "../../../testdata/interp/p4/p4c/includes" ]; filename; stf }
  in
  Spectec.eval_task (module P4.Sim) ~check_determinism:true ~mode ~spec_il input

let check_simulation spec_il mode filename ~stf =
  simulate spec_il mode filename ~stf |> unwrap |> ignore

let check_malformed spec_il mode =
  match
    simulate spec_il mode "sim-operations.p4"
      ~stf:"sim-operations-malformed.stf"
  with
  | Error (Spectec.Error.TaskParseError (_, message))
    when P4.contains_substring message "malformed expect statement" ->
      ()
  | Ok _ -> failwith "a malformed expect line was accepted"
  | Error error -> failwith (render error)

let check_mismatch spec_il mode =
  match
    simulate spec_il mode "sim-operations.p4" ~stf:"sim-operations-mismatch.stf"
  with
  | Error (Spectec.Error.TaskError (_, message))
    when P4.contains_substring message "but expected" ->
      ()
  | Ok _ -> failwith "a mismatched expect line was accepted"
  | Error error -> failwith (render error)

let check_nested_store mode = function
  | [ _global; store ] ->
      let objects =
        B.Arg.map Common.Location.Unknown store
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

let check_field_location () =
  let program = P4.Frontend.parse_file [] "included-field.p4" |> unwrap in
  let rec fields (value : V.t) =
    match value.it with
    | Lang.Il.CaseV notation ->
        let children = Lang.Il.Mixfix.args notation in
        let nested = List.concat_map fields children in
        if
          V.id_of_case_v value = "typeField"
          || (V.id_of_case_v value = "typeFieldList" && children <> [])
        then value :: nested
        else nested
    | Lang.Il.ListV values -> List.concat_map fields values
    | Lang.Il.OptV (Some value) -> fields value
    | _ -> []
  in
  let check (field : V.t) =
    match Common.Location.file field.at with
    | Some file when Filename.basename file = "included-field-fragment.p4" -> ()
    | _ ->
        failwith
          ("incorrect field location: " ^ Common.Location.to_string field.at)
  in
  match fields program with
  | [ field_list; field ] -> List.iter check [ field_list; field ]
  | _ -> failwith "expected one included field and its list"

let check_min_nat () =
  let builtins = B.make P4.Builtins.builtins in
  let nats = List.map (fun n -> V.nat (Bigint.of_int n)) [ 3; 5 ] in
  match
    builtins.invoke
      ~callback:(fun _ _ _ -> failwith "min_nat does not call back")
      ("min_nat" $ at) []
      [ V.list (Lang.Il.Typ.nat $ at) nats ]
  with
  | Ok value when V.eq value (V.nat (Bigint.of_int 3)) -> ()
  | _ -> failwith "min_nat did not return the smallest element"

let () =
  check_field_location ();
  check_min_nat ();
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
      check_simulation spec_il mode
        (samples ^ "valid_ebpf.p4")
        ~stf:(samples ^ "valid_ebpf.stf");
      check_simulation spec_il mode "sim-operations.p4"
        ~stf:"sim-operations.stf";
      match run spec_il mode "unsupported.p4" with
      | Ok _ -> failwith (name ^ ": unsupported object was instantiated")
      | Error error ->
          let diagnostics = render error in
          if
            not
              (P4.contains_substring diagnostics
                 "unsupported eBPF extern object: Unsupported")
          then failwith diagnostics)
    [ ("IL", Spectec.Interp_mode.Il); ("SL", Sl); ("PL", Pl henv) ];
  check_mismatch spec_il Spectec.Interp_mode.Sl;
  check_malformed spec_il Spectec.Interp_mode.Sl
