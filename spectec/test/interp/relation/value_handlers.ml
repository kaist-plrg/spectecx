module H = Harness
module V = Lang.Il.Value
module Value_event = Instrumentation.Value_event

let ( $ ) = Common.Annotated.( $ )
let nat n = V.nat (Bigint.of_int n)

let field name value =
  (Lang.Xl.Atom.keyword name $ Common.Location.Unknown, value)

let point x y = V.record "point" [ field "X" (nat x); field "Y" (nat y) ]

let nats ns =
  V.list (Lang.Il.Typ.nat $ Common.Location.Unknown) (List.map nat ns)

let spec = H.compile_file "value_handlers.spectec"

type input = { relation : string; decode : unit -> (V.t list, string) result }

module Target : Spectec.Target.S = struct
  include H.Pure_target

  let builtins =
    [
      ( "double",
        Interp.Builtins.Define.T0.a1 Interp.Builtins.Arg.nat (fun ~at:_ n ->
            Ok (V.nat Bigint.(n + n))) );
    ]

  let name = "value-handlers"
  let spec_dir = "."
end

module Task : Spectec.Task.S with type input = input = struct
  let name = "evaluate"

  module Target = Target

  type nonrec input = input

  let parse_string ~spec:_ ~filename:_ _ = Ok []
  let unparse ~spec:_ _ = ""

  let parse_input ~spec:_ { relation; decode } =
    match decode () with
    | Ok values -> Ok (Spectec.Task.Relation (relation, values))
    | Error message ->
        Error (Spectec.Error.TaskParseError (Common.Location.Unknown, message))

  let source { relation; _ } = relation
  let expectation _ = Spectec.Task.Positive
  let collect ?dir:_ () = []
  let format_output _ = ""
  let save_output _ _ = ()
end

let input relation values = { relation; decode = (fun () -> Ok (values ())) }

let modes =
  [
    ("IL", Spectec.Interp_mode.Il);
    ("SL", Spectec.Interp_mode.Sl);
    ("PL", Spectec.Interp_mode.Pl spec.henv);
  ]

let evaluate ?config mode input =
  Spectec.eval_task_with_instrumentation
    (module Task)
    ?config ~mode ~spec_il:spec.spec_il input

let fail_check ~name ~mode_name message =
  failwith (Printf.sprintf "%s (%s): %s" name mode_name message)

let values_exn ~name ~mode_name = function
  | Ok values -> values
  | Error error ->
      fail_check ~name ~mode_name
        ("evaluation failed:\n"
        ^ Spectec.Diagnostic.Render.render_bag
            ~ansi:Spectec.Diagnostic.Ansi.plain
            (Spectec.Error.to_diagnostics error))

let handler_config ~name ?handle_value ?(finish = fun () -> ()) () :
    Instrumentation.Handler.Config.t =
  let module Handler = struct
    let static_dependencies = []
    let init ~spec:_ = ()
    let handle _ = ()
    let handle_value = handle_value
    let finish = finish
  end in
  {
    name;
    modes = [ `IL; `SL; `PL ];
    handler = (module Handler);
    output = Instrumentation.Output.stdout;
  }

(* Records each event with the name of the handler that received it, newest
   first. *)
let recorder ~name ~log =
  handler_config ~name
    ~handle_value:(fun event -> log := (name, event) :: !log)
    ()

let recorded_events ~name ~mode input =
  let log = ref [] in
  let mode_name, mode = mode in
  ignore
    (evaluate ~config:[ recorder ~name:"recorder" ~log ] mode input
    |> values_exn ~name ~mode_name);
  List.rev_map snd !log

let sum_twice () = input "Sum_twice" (fun () -> [ point 1 2; point 1 2 ])
let shift () = input "Shift" (fun () -> [ point 1 2; nats [ 5; 6 ] ])
let offset_twice () = input "Offset_twice" (fun () -> [ nat 1; nat 2 ])

let handlers_preserve_results () =
  let name = "observed and unobserved runs return equal values" in
  List.iter
    (fun (mode_name, mode) ->
      List.iter
        (fun input ->
          let unobserved = evaluate mode input |> values_exn ~name ~mode_name in
          let log = ref [] in
          let observed =
            evaluate ~config:[ recorder ~name:"recorder" ~log ] mode input
            |> values_exn ~name ~mode_name
          in
          if !log = [] then fail_check ~name ~mode_name "no event was observed";
          if not (List.equal V.eq unobserved observed) then
            fail_check ~name ~mode_name "observed run returned other values")
        [ sum_twice (); shift () ])
    modes

let composed_handlers_receive_events_in_order () =
  let name = "composed handlers receive each event in configured order" in
  List.iter
    (fun (mode_name, mode) ->
      let log = ref [] in
      let config =
        [ recorder ~name:"first" ~log; recorder ~name:"second" ~log ]
      in
      ignore (evaluate ~config mode (shift ()) |> values_exn ~name ~mode_name);
      let rec check_pairs = function
        | [] -> ()
        | ("first", event_first) :: ("second", event_second) :: rest
          when event_first == event_second ->
            check_pairs rest
        | _ ->
            fail_check ~name ~mode_name
              "events did not reach both handlers in configured order"
      in
      check_pairs (List.rev !log))
    modes

let cache_hits_are_reported () =
  let name = "repeated calls with equal arguments report a cache hit" in
  List.iter
    (fun mode ->
      List.iter
        (fun (input, callee) ->
          let cached =
            recorded_events ~name ~mode input
            |> List.filter_map (function
                 | Value_event.Invoked event when event.callee = callee ->
                     Some event.cached
                 | _ -> None)
          in
          if cached <> [ false; true ] then
            fail_check ~name ~mode_name:(fst mode)
              ("unexpected cache reports for " ^ input.relation))
        [
          (sum_twice (), Value_event.Func "sum");
          (offset_twice (), Value_event.Rel "Add_offset");
        ])
    modes

let check_result_events ~relation ~args ~check =
  let name = relation ^ " reports its value sources" in
  List.iter
    (fun (mode_name, mode) ->
      let inputs = ref [] in
      let input =
        input relation (fun () ->
            let values = args () in
            inputs := values;
            values)
      in
      let log = ref [] in
      let outputs =
        evaluate ~config:[ recorder ~name:"recorder" ~log ] mode input
        |> values_exn ~name ~mode_name
      in
      match outputs with
      | [ output ] ->
          if not (check ~inputs:!inputs ~output ~events:(List.rev_map snd !log))
          then fail_check ~name ~mode_name "missing or incorrect event payload"
      | _ -> fail_check ~name ~mode_name "expected one result")
    modes

let calls_report_inputs_and_outputs () =
  List.iter
    (fun (relation, callee, args) ->
      check_result_events ~relation ~args ~check:(fun ~inputs ~output ~events ->
          List.exists
            (function
              | Value_event.Invoked
                  {
                    callee = callee_called;
                    inputs = inputs_called;
                    outputs = [ value ];
                    cached = false;
                  } ->
                  callee_called = callee && value == output
                  && List.equal ( == ) inputs_called inputs
              | _ -> false)
            events))
    [
      ("Shift", Value_event.Rel "Shift", fun () -> [ point 1 2; nats [ 5; 6 ] ]);
      ("Double", Value_event.Func "double", fun () -> [ nat 4 ]);
    ]

let unary_operations_report_their_operand () =
  List.iter
    (fun (relation, args) ->
      check_result_events ~relation ~args ~check:(fun ~inputs ~output ~events ->
          match inputs with
          | [ source ] ->
              List.exists
                (function
                  | Value_event.Derived event ->
                      event.value == output && event.source == source
                  | _ -> false)
                events
          | _ -> false))
    [
      ("Length", fun () -> [ nats [ 5; 6 ] ]);
      ("Not", fun () -> [ V.bool true ]);
      ("Is_nat", fun () -> [ V.int (Bigint.of_int 3) ]);
    ]

let pattern_tests_report_their_operand () =
  check_result_events ~relation:"Is_on"
    ~args:(fun () ->
      [
        V.Make.case
          (Lang.Il.Typ.var "switch" [])
          [ Atom (Lang.Xl.Atom.keyword "ON" $ Common.Location.Unknown) ];
      ])
    ~check:(fun ~inputs ~output:_ ~events ->
      match inputs with
      | [ source ] ->
          List.exists
            (function
              | Value_event.Derived
                  { value = { it = BoolV true; _ }; source = s } ->
                  s == source
              | _ -> false)
            events
      | _ -> false)

let constructors_report_components () =
  List.iter
    (fun (relation, args) ->
      check_result_events ~relation ~args ~check:(fun ~inputs ~output ~events ->
          List.exists
            (function
              | Value_event.Constructed { value; components } ->
                  value == output && List.equal ( == ) inputs components
              | _ -> false)
            events))
    [
      ("Make_point", fun () -> [ nat 1; nat 2 ]);
      ("Make_pair", fun () -> [ nat 1; nat 2 ]);
      ("Wrap", fun () -> [ nat 1 ]);
      ("Make_list", fun () -> [ nat 1; nat 2 ]);
      ("Make_option", fun () -> [ nat 1 ]);
    ]

let operations_report_their_operands () =
  List.iter
    (fun (relation, args) ->
      check_result_events ~relation ~args ~check:(fun ~inputs ~output ~events ->
          List.exists
            (function
              | Value_event.Combined { value; sources } ->
                  value == output && List.equal ( == ) sources inputs
              | _ -> false)
            events))
    [
      ("Less", fun () -> [ nat 1; nat 2 ]);
      ("Concat", fun () -> [ nats [ 1 ]; nats [ 2; 3 ] ]);
      ("Member", fun () -> [ nat 2; nats [ 1; 2 ] ]);
      ("Prepend", fun () -> [ nat 1; nats [ 2; 3 ] ]);
      ("Text_index", fun () -> [ V.text "abcde"; nat 1 ]);
      ("Text_slice", fun () -> [ V.text "abcde"; nat 1; nat 2 ]);
      ("List_slice", fun () -> [ nats [ 2; 4; 6; 8 ]; nat 1; nat 2 ]);
    ]

let casts_report_rebuilt_values () =
  let typ_nat = Lang.Il.Typ.nat $ Common.Location.Unknown in
  let typ_int = Lang.Il.Typ.int $ Common.Location.Unknown in
  let ints ns =
    V.list typ_int (List.map (fun n -> V.int (Bigint.of_int n)) ns)
  in
  List.iter
    (fun (relation, args) ->
      check_result_events ~relation ~args ~check:(fun ~inputs ~output ~events ->
          let rec reported (value : V.t) ~(source : V.t) =
            value != source
            && List.exists
                 (function
                   | Value_event.Derived event ->
                       event.value == value && event.source == source
                   | _ -> false)
                 events
            &&
            match (source.it, value.it) with
            | TupleV sources, TupleV values | ListV sources, ListV values ->
                List.length sources = List.length values
                && List.for_all2
                     (fun source value -> reported value ~source)
                     sources values
            | OptV None, OptV None -> true
            | OptV (Some source), OptV (Some value) -> reported value ~source
            | NumV _, NumV _ -> true
            | _ -> false
          in
          match inputs with [ source ] -> reported output ~source | _ -> false))
    [
      ("Widen", fun () -> [ nat 3 ]);
      ("Narrow", fun () -> [ V.int (Bigint.of_int 3) ]);
      ("Widen_pair", fun () -> [ V.tuple [ nat 3; nat 4 ] ]);
      ( "Narrow_pair",
        fun () ->
          [ V.tuple [ V.int (Bigint.of_int 3); V.int (Bigint.of_int 4) ] ] );
      ("Widen_list", fun () -> [ nats [ 3; 4 ] ]);
      ("Widen_list", fun () -> [ nats [] ]);
      ("Narrow_list", fun () -> [ ints [ 3; 4 ] ]);
      ("Narrow_list", fun () -> [ ints [] ]);
      ("Widen_option", fun () -> [ V.opt typ_nat (Some (nat 3)) ]);
      ("Widen_option", fun () -> [ V.opt typ_nat None ]);
      ( "Narrow_option",
        fun () -> [ V.opt typ_int (Some (V.int (Bigint.of_int 3))) ] );
      ("Narrow_option", fun () -> [ V.opt typ_int None ]);
    ]

let patterns_report_derived_values () =
  check_result_events ~relation:"Tail"
    ~args:(fun () -> [ nats [ 1; 2; 3 ] ])
    ~check:(fun ~inputs ~output ~events ->
      match inputs with
      | [ source ] ->
          List.exists
            (function
              | Value_event.Derived event ->
                  event.value == output
                  && List.equal ( == ) (V.get_list event.source)
                       (V.get_list source)
                  && List.equal ( == ) (V.get_list output)
                       (List.tl (V.get_list source))
              | _ -> false)
            events
      | _ -> false);
  List.iter
    (fun (relation, args) ->
      check_result_events ~relation ~args ~check:(fun ~inputs ~output ~events ->
          let contents (value : V.t) =
            match value.it with
            | ListV values -> values
            | OptV value -> Option.to_list value
            | _ -> failwith "expected a list or option"
          in
          match inputs with
          | [ source ] ->
              List.exists
                (function
                  | Value_event.Derived event ->
                      event.source == source && event.value != source
                      && V.eq event.value source
                      && List.equal ( == ) (contents event.value)
                           (contents output)
                  | _ -> false)
                events
          | _ -> false))
    [
      ("Pattern_list", fun () -> [ nats [ 1; 2 ] ]);
      ( "Pattern_option",
        fun () ->
          [ V.opt (Lang.Il.Typ.nat $ Common.Location.Unknown) (Some (nat 1)) ]
      );
      ( "Pattern_option",
        fun () -> [ V.opt (Lang.Il.Typ.nat $ Common.Location.Unknown) None ] );
    ]

let nested_updates_report_each_container () =
  check_result_events ~relation:"Replace_nested"
    ~args:(fun () ->
      [ V.record "envelope" [ field "INNER" (point 1 2) ]; nat 9 ])
    ~check:(fun ~inputs ~output ~events ->
      match inputs with
      | [ base; replacement ] ->
          let original_inner = V.get_struct base |> List.hd |> snd in
          let updated_inner = V.get_struct output |> List.hd |> snd in
          let reports ~value ~base ~replacement =
            List.exists
              (function
                | Value_event.Updated event ->
                    event.value == value && event.base == base
                    && event.replacement == replacement
                | _ -> false)
              events
          in
          reports ~value:updated_inner ~base:original_inner ~replacement
          && reports ~value:output ~base ~replacement:updated_inner
      | _ -> false)

let slice_updates_report_base_and_replacement () =
  List.iter
    (fun (relation, args) ->
      check_result_events ~relation ~args ~check:(fun ~inputs ~output ~events ->
          match inputs with
          | [ base; replacement ] ->
              List.exists
                (function
                  | Value_event.Updated event ->
                      event.value == output && event.base == base
                      && event.replacement == replacement
                  | _ -> false)
                events
          | _ -> false))
    [
      ("Replace_text_slice", fun () -> [ V.text "abcd"; V.text "xy" ]);
      ("Replace_list_slice", fun () -> [ nats [ 2; 4; 6; 8 ]; nats [ 9; 9 ] ]);
    ]

let update_paths_report_intermediate_values () =
  List.iter
    (fun (relation, args) ->
      check_result_events ~relation ~args
        ~check:(fun ~inputs ~output:_ ~events ->
          let read_from container (part : V.t) =
            List.exists
              (function
                | Value_event.Combined { value; sources = source :: _ } ->
                    value == part && source == container
                | _ -> false)
              events
          in
          match inputs with
          | [ container; replacement ] ->
              List.exists
                (function
                  | Value_event.Updated updated ->
                      updated.replacement == replacement
                      && read_from container updated.base
                  | _ -> false)
                events
          | _ -> false))
    [
      ("Replace_in_slice", fun () -> [ nats [ 2; 4; 6; 8 ]; nat 9 ]);
      ("Replace_in_char", fun () -> [ V.text "abc"; V.text "z" ]);
    ]

let iteration_events_include_body_results () =
  let name = "iteration events include their input and produced values" in
  let option content () =
    V.opt (Lang.Il.Typ.nat $ Common.Location.Unknown) (Option.map nat content)
  in
  List.iter
    (fun (mode_name, mode) ->
      List.iter
        (fun (relation, make_ranged) ->
          let ranged = ref None in
          let input =
            input relation (fun () ->
                let value = make_ranged () in
                ranged := Some value;
                [ value; nat 10 ])
          in
          let log = ref [] in
          let result =
            match
              evaluate ~config:[ recorder ~name:"recorder" ~log ] mode input
              |> values_exn ~name ~mode_name
            with
            | [ value ] -> value
            | _ -> fail_check ~name ~mode_name "expected one iteration result"
          in
          let produced =
            match result.it with
            | ListV values -> values
            | OptV content -> Option.to_list content
            | _ -> fail_check ~name ~mode_name "expected a list or option"
          in
          let complete =
            List.exists
              (function
                | _, Value_event.Combined { sources; _ } ->
                    List.exists (V.eq (Option.get !ranged)) sources
                    && List.for_all
                         (fun expected ->
                           List.exists
                             (fun source -> source == expected)
                             sources)
                         produced
                | _ -> false)
              !log
          in
          if not complete then
            fail_check ~name ~mode_name
              ("missing iteration inputs or produced values for " ^ relation))
        [
          ("Offset_map", fun () -> nats [ 1; 2 ]);
          ("Offset_let", fun () -> nats [ 1; 2 ]);
          ("Offset_rel", fun () -> nats [ 1; 2 ]);
          ("Offset_opt", option (Some 1));
          ("Offset_opt", option None);
        ])
    modes

exception Handler_failed

let handler_failure_finishes_handlers () =
  let name = "a handler exception finishes the session" in
  List.iter
    (fun (mode_name, mode) ->
      let finished = ref false in
      let failing =
        handler_config ~name:"failing"
          ~finish:(fun () -> finished := true)
          ~handle_value:(fun _ -> raise Handler_failed)
          ()
      in
      (match evaluate ~config:[ failing ] mode (shift ()) with
      | exception Handler_failed -> ()
      | _ -> fail_check ~name ~mode_name "the handler exception was lost");
      if not !finished then
        fail_check ~name ~mode_name "the failing handler was not finished";
      let log = ref [] in
      ignore
        (evaluate ~config:[ recorder ~name:"recorder" ~log ] mode (shift ())
        |> values_exn ~name ~mode_name))
    modes

let decode_failures_finish_handlers () =
  let name = "decoding failures finish handlers" in
  List.iter
    (fun (mode_name, mode) ->
      let finished = ref 0 in
      let config =
        [ handler_config ~name:"counter" ~finish:(fun () -> incr finished) () ]
      in
      (match
         evaluate ~config mode
           { relation = "Shift"; decode = (fun () -> Error "malformed") }
       with
      | Error (Spectec.Error.TaskParseError (_, "malformed")) -> ()
      | _ -> fail_check ~name ~mode_name "the parse error was not returned");
      (match
         evaluate ~config mode
           { relation = "Shift"; decode = (fun () -> raise Exit) }
       with
      | exception Exit -> ()
      | _ -> fail_check ~name ~mode_name "the decoder exception was lost");
      if !finished <> 2 then
        fail_check ~name ~mode_name
          "the handler was not finished after each run")
    modes

(* Emits [Decoded] for each labeled value and for each struct field and list
   element inside it, as a decoder that labels each of its inputs would. *)
let report_decoded labeled_values =
  let rec report (value : V.t) origin =
    Instrumentation.Dispatcher.emit_value (Decoded { value; origin });
    match value.it with
    | StructV fields ->
        List.iter
          (fun ((atom : Lang.Il.atom), field) ->
            report field
              (Instrumentation.Origin.extend origin ~step:(Struct atom.it)))
          fields
    | ListV values ->
        List.iteri
          (fun index value ->
            report value
              (Instrumentation.Origin.extend origin ~step:(List index)))
          values
    | _ -> ()
  in
  List.map
    (fun (label, value) ->
      report value (Instrumentation.Origin.make ~label);
      value)
    labeled_values

let decoded_fields_reach_later_events () =
  let name = "decoded struct fields reach later events" in
  let origin_x : Instrumentation.Origin.t =
    { label = "point"; path = [ Struct (Lang.Xl.Atom.keyword "X") ] }
  in
  List.iter
    (fun (mode_name, mode) ->
      let origins = ref [] in
      let reached = ref false in
      let handle_value : Value_event.t -> unit = function
        | Decoded { value; origin } -> origins := (value, origin) :: !origins
        | Combined { sources; _ } ->
            if
              List.exists
                (fun source -> List.assq_opt source !origins = Some origin_x)
                sources
            then reached := true
        | _ -> ()
      in
      let input =
        {
          relation = "Shift";
          decode =
            (fun () ->
              Ok
                (report_decoded
                   [ ("point", point 1 2); ("lengths", nats [ 5 ]) ]));
        }
      in
      ignore
        (evaluate
           ~config:[ handler_config ~name:"origins" ~handle_value () ]
           mode input
        |> values_exn ~name ~mode_name);
      if not !reached then
        fail_check ~name ~mode_name "no computed value read the X field")
    modes

let run () =
  handlers_preserve_results ();
  composed_handlers_receive_events_in_order ();
  cache_hits_are_reported ();
  calls_report_inputs_and_outputs ();
  unary_operations_report_their_operand ();
  pattern_tests_report_their_operand ();
  constructors_report_components ();
  operations_report_their_operands ();
  casts_report_rebuilt_values ();
  patterns_report_derived_values ();
  nested_updates_report_each_container ();
  slice_updates_report_base_and_replacement ();
  update_paths_report_intermediate_values ();
  iteration_events_include_body_results ();
  handler_failure_finishes_handlers ();
  decode_failures_finish_handlers ();
  decoded_fields_reach_later_events ()
