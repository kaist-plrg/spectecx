module H = Harness
module V = Lang.Il.Value
module Value_event = Instrumentation.Value_event

let ( $ ) = Common.Source.( $ )
let nat n = V.nat (Bigint.of_int n)

let field name value =
  (Lang.Xl.Atom.keyword name $ Common.Source.no_region, value)

let point x y = V.record "point" [ field "X" (nat x); field "Y" (nat y) ]

let nats ns =
  V.list (Lang.Il.Typ.nat $ Common.Source.no_region) (List.map nat ns)

let spec = H.compile_file "value_observers.spectec"

type input = { relation : string; decode : unit -> (V.t list, string) result }

module Target : Spectec.Target.S = struct
  include H.Pure_target

  (* [$wrap] builds every value of its result, so the interpreter reports none
     of them. *)
  let builtins =
    [
      ( "wrap",
        Interp.Builtins.Define.T0.a1 Interp.Builtins.Arg.nat (fun ~at:_ n ->
            Ok (V.record "point" [ field "X" (V.nat n); field "Y" (nat 0) ])) );
    ]

  let name = "value-observers"
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
    | Ok values -> Ok (relation, values)
    | Error message ->
        Error (Spectec.Error.TaskParseError (Common.Source.no_region, message))

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

let handler_config ~name ?(finish = fun () -> ()) observe :
    Instrumentation.Handler.Config.t =
  let module Observer = struct
    let static_dependencies = []
    let init ~spec:_ = ()
    let handle _ = ()
    let observe_value = Some observe
    let finish = finish
  end in
  {
    name;
    modes = [ `IL; `SL; `PL ];
    handler = (module Observer);
    output = Instrumentation.Output.stdout;
  }

(* Records each event with the name of the handler that received it, newest
   first. *)
let recorder ~name ~log =
  handler_config ~name (fun event -> log := (name, event) :: !log)

let describe : Value_event.t -> string = function
  | Created _ -> "created"
  | Derived _ -> "derived"
  | Combined _ -> "combined"
  | Updated _ -> "updated"
  | Invoked { callee; cached; _ } ->
      let callee = match callee with Func id -> "$" ^ id | Rel id -> id in
      if cached then "invoked " ^ callee ^ " from cache"
      else "invoked " ^ callee

let recorded_events ~name ~mode input =
  let log = ref [] in
  let mode_name, mode = mode in
  ignore
    (evaluate ~config:[ recorder ~name:"recorder" ~log ] mode input
    |> values_exn ~name ~mode_name);
  List.rev_map snd !log

let sum_twice () = input "Sum_twice" (fun () -> [ point 1 2; point 1 2 ])
let shift () = input "Shift" (fun () -> [ point 1 2; nats [ 5; 6 ] ])

let observers_preserve_results () =
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

let composed_observers_receive_events_in_order () =
  let name = "composed observers receive each event in configured order" in
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
              "events did not reach both observers in configured order"
      in
      check_pairs (List.rev !log))
    modes

let cache_hits_are_reported () =
  let name = "repeated calls with equal arguments report a cache hit" in
  List.iter
    (fun mode ->
      let calls =
        recorded_events ~name ~mode (sum_twice ())
        |> List.map describe
        |> List.filter (String.starts_with ~prefix:"invoked $sum")
      in
      if calls <> [ "invoked $sum"; "invoked $sum from cache" ] then
        fail_check ~name ~mode_name:(fst mode)
          ("unexpected calls: " ^ String.concat ", " calls))
    modes

let expression_events_reach_every_mode () =
  let name = "length, addition and field update report their results" in
  List.iter
    (fun mode ->
      let kinds = recorded_events ~name ~mode (shift ()) |> List.map describe in
      List.iter
        (fun kind ->
          if not (List.mem kind kinds) then
            fail_check ~name ~mode_name:(fst mode) ("missing " ^ kind))
        [ "derived"; "combined"; "updated"; "invoked Shift" ])
    modes

let iteration_events_include_body_results () =
  let name = "iteration events include their input and produced values" in
  let option content () =
    V.opt (Lang.Il.Typ.nat $ Common.Source.no_region) (Option.map nat content)
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

exception Observer_failed

let observer_failure_finishes_handlers () =
  let name = "an observer exception finishes the session" in
  List.iter
    (fun (mode_name, mode) ->
      let finished = ref false in
      let failing =
        handler_config ~name:"failing"
          ~finish:(fun () -> finished := true)
          (fun _ -> raise Observer_failed)
      in
      (match evaluate ~config:[ failing ] mode (shift ()) with
      | exception Observer_failed -> ()
      | _ -> fail_check ~name ~mode_name "the observer exception was lost");
      if not !finished then
        fail_check ~name ~mode_name "the failing handler was not finished";
      let log = ref [] in
      ignore
        (evaluate ~config:[ recorder ~name:"recorder" ~log ] mode (shift ())
        |> values_exn ~name ~mode_name))
    modes

(* Emits [Created] for each argument with its own source label, as a decoder
   that labels its input does. *)
let seed labeled_values =
  List.map
    (fun (source, value) ->
      Instrumentation.Dispatcher.emit_value
        (Created { value; origin = Instrumentation.Origin.make ~source });
      value)
    labeled_values

let decoded_values_reach_observers () =
  let name = "values created while decoding reach observers" in
  List.iter
    (fun mode ->
      let input =
        {
          relation = "Shift";
          decode =
            (fun () ->
              Ok (seed [ ("point", point 1 2); ("lengths", nats [ 5 ]) ]));
        }
      in
      let created =
        recorded_events ~name ~mode input
        |> List.filter (function Value_event.Created _ -> true | _ -> false)
      in
      if List.length created <> 2 then
        fail_check ~name ~mode_name:(fst mode)
          "decoder events did not reach the observer")
    modes

let decode_failures_finish_handlers () =
  let name = "decoding failures finish handlers" in
  List.iter
    (fun (mode_name, mode) ->
      let finished = ref 0 in
      let config =
        [
          handler_config ~name:"counter"
            ~finish:(fun () -> incr finished)
            ignore;
        ]
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

module Origin = Instrumentation.Origin
module Origin_tracking = Instrumentation.Origin_tracking

let rendered_origins tracker values =
  List.concat_map (fun value -> Origin_tracking.origins tracker ~value) values
  |> List.map Origin.to_string
  |> List.sort_uniq String.compare

let evaluate_tracked ~name ~tracker (mode_name, mode) input =
  evaluate ~config:[ Origin_tracking.handler_config tracker ] mode input
  |> values_exn ~name ~mode_name

let check_origins ~name ~mode_name ~expected actual =
  if actual <> expected then
    fail_check ~name ~mode_name
      (Printf.sprintf "expected origins [%s] but got [%s]"
         (String.concat "; " expected)
         (String.concat "; " actual))

let labeled_input relation labeled_values =
  { relation; decode = (fun () -> Ok (seed (labeled_values ()))) }

let holder () =
  let typ name = Lang.Il.Typ.var name [] $ Common.Source.no_region in
  V.record "holder"
    [
      field "POINTS" (V.list (typ "point") [ point 1 2; point 3 4 ]);
      field "PAIR" (V.tuple [ nat 5; nat 6 ]);
      field "SHAPE"
        (Lang.Il.Case.case_v ~var:"shape"
           Lang.Il.Case.[ kw "RECT"; arg (nat 7); arg (nat 8) ]);
      field "MAYBE"
        (V.opt (Lang.Il.Typ.nat $ Common.Source.no_region) (Some (nat 9)));
    ]

let find_field (value : V.t) name =
  match value.it with
  | StructV fields ->
      List.find
        (fun ((atom : Lang.Il.atom), _) ->
          Lang.Xl.Atom.to_string atom.it = name)
        fields
      |> snd
  | _ -> failwith "expected a struct"

let updated_fields_keep_origins () =
  let name = "field updates preserve base and replacement origins" in
  List.iter
    (fun mode ->
      let mode_name = fst mode in
      let tracker = Origin_tracking.make () in
      let result =
        labeled_input "Shift" (fun () ->
            [ ("point", point 1 2); ("lengths", nats [ 5; 6 ]) ])
        |> evaluate_tracked ~name ~tracker mode
      in
      match result with
      | [ value ] ->
          rendered_origins tracker [ value ]
          |> check_origins ~name ~mode_name
               ~expected:
                 [ "lengths"; "lengths[0]"; "lengths[1]"; "point"; "point.X" ];
          rendered_origins tracker [ find_field value "X" ]
          |> check_origins ~name ~mode_name
               ~expected:[ "lengths"; "lengths[0]"; "lengths[1]"; "point.X" ];
          rendered_origins tracker [ find_field value "Y" ]
          |> check_origins ~name ~mode_name ~expected:[ "point.Y" ]
      | _ -> fail_check ~name ~mode_name "expected one updated record")
    modes

let origins_follow_input_structure () =
  let name = "origins follow struct, list, tuple, case and option paths" in
  List.iter
    (fun mode ->
      let mode_name = fst mode in
      let tracker = Origin_tracking.make () in
      let decoded = ref None in
      let input =
        labeled_input "Second_y" (fun () ->
            let value = holder () in
            decoded := Some value;
            [ ("holder", value) ])
      in
      evaluate_tracked ~name ~tracker mode input
      |> rendered_origins tracker
      |> check_origins ~name ~mode_name ~expected:[ "holder.POINTS[1].Y" ];
      let holder = Option.get !decoded in
      let nested =
        match
          ( (find_field holder "PAIR").it,
            (find_field holder "SHAPE").it,
            (find_field holder "MAYBE").it )
        with
        | TupleV [ _; second ], CaseV valuecase, OptV (Some content) ->
            [ second; List.nth (Lang.Il.Mixfix.args valuecase) 1; content ]
        | _ -> failwith "unexpected holder layout"
      in
      rendered_origins tracker nested
      |> check_origins ~name ~mode_name
           ~expected:[ "holder.MAYBE?"; "holder.PAIR.1"; "holder.SHAPE#1" ])
    modes

let origins_combine_through_calls () =
  let name = "results report the input fields they are computed from" in
  List.iter
    (fun mode ->
      let mode_name = fst mode in
      let tracker = Origin_tracking.make () in
      let input relation =
        labeled_input relation (fun () ->
            [ ("left", point 1 2); ("right", point 3 4) ])
      in
      evaluate_tracked ~name ~tracker mode (input "Sum_twice")
      |> rendered_origins tracker
      |> check_origins ~name ~mode_name
           ~expected:[ "left.X"; "left.Y"; "right.X"; "right.Y" ];
      evaluate_tracked ~name ~tracker mode (input "Copy_both")
      |> rendered_origins tracker
      |> check_origins ~name ~mode_name ~expected:[ "left.X"; "right.Y" ])
    modes

let cache_hits_keep_current_origins () =
  let name = "a cached call reports the origins of its current arguments" in
  List.iter
    (fun mode ->
      let mode_name = fst mode in
      List.iter
        (fun relation ->
          let tracker = Origin_tracking.make () in
          let origins =
            labeled_input relation (fun () ->
                [ ("left", point 1 2); ("right", point 1 2) ])
            |> evaluate_tracked ~name ~tracker mode
            |> rendered_origins tracker
          in
          if
            not
              (List.exists (String.starts_with ~prefix:"right") origins
              && List.exists (String.starts_with ~prefix:"left") origins)
          then
            fail_check ~name ~mode_name
              (Printf.sprintf "%s reported [%s]" relation
                 (String.concat "; " origins)))
        [ "Sum_twice"; "Copy_both" ])
    modes

let has_only_sources ~prefix origins =
  origins <> [] && List.for_all (String.starts_with ~prefix) origins

let repeated_runs_do_not_share_origins () =
  let name = "a new run does not report origins of an earlier run" in
  List.iter
    (fun mode ->
      let mode_name = fst mode in
      let tracker = Origin_tracking.make () in
      let run prefix =
        labeled_input "Sum_twice" (fun () ->
            [ (prefix ^ "-left", point 1 2); (prefix ^ "-right", point 3 4) ])
        |> evaluate_tracked ~name ~tracker mode
        |> rendered_origins tracker
      in
      ignore (run "first");
      let origins = run "second" in
      if not (has_only_sources ~prefix:"second-" origins) then
        fail_check ~name ~mode_name
          ("second run reported " ^ String.concat "; " origins))
    modes

let batch_test_cases_do_not_share_origins () =
  let name = "a batch test case does not report origins of an earlier one" in
  List.iter
    (fun (mode_name, mode) ->
      let tracker = Origin_tracking.make () in
      let input prefix =
        labeled_input "Sum_twice" (fun () ->
            [ (prefix ^ "-left", point 1 2); (prefix ^ "-right", point 3 4) ])
      in
      let results =
        Batch.run_batch_with_outcomes
          (module Task)
          ~config:[ Origin_tracking.handler_config tracker ]
          ~ansi:Spectec.Diagnostic.Ansi.plain ~mode ~spec_il:spec.spec_il
          [ input "first"; input "second" ]
      in
      match List.rev results with
      | { outcome = Spectec.Task.Pass values; _ } :: _ ->
          let origins = rendered_origins tracker values in
          if not (has_only_sources ~prefix:"second-" origins) then
            fail_check ~name ~mode_name
              ("second test case reported " ^ String.concat "; " origins)
      | _ -> fail_check ~name ~mode_name "the second test case did not pass")
    modes

let check_all checks =
  match
    List.filter_map
      (fun check ->
        match check () with
        | () -> None
        | exception Failure message -> Some message)
      checks
  with
  | [] -> ()
  | failures -> failwith (String.concat "\n" failures)

let builtin_results_report_their_arguments () =
  let name = "parts of a builtin result report the builtin's arguments" in
  check_all
    (List.map
       (fun mode () ->
         let tracker = Origin_tracking.make () in
         labeled_input "Wrapped_sum" (fun () ->
             [ ("count", nat 3); ("offset", nat 4) ])
         |> evaluate_tracked ~name ~tracker mode
         |> rendered_origins tracker
         |> check_origins ~name ~mode_name:(fst mode)
              ~expected:[ "count"; "offset" ])
       modes)

let iteration_results_report_body_inputs () =
  let name = "an iteration result reports the inputs its body reads" in
  check_all
    (List.concat_map
       (fun mode ->
         List.map
           (fun relation () ->
             let tracker = Origin_tracking.make () in
             labeled_input relation (fun () ->
                 [ ("numbers", nats [ 1; 2 ]); ("offset", nat 10) ])
             |> evaluate_tracked ~name ~tracker mode
             |> rendered_origins tracker
             |> check_origins
                  ~name:(name ^ ", " ^ relation)
                  ~mode_name:(fst mode)
                  ~expected:[ "numbers"; "numbers[0]"; "numbers[1]"; "offset" ])
           [ "Offset_map"; "Offset_let"; "Offset_rel" ]
         @ List.map
             (fun (content, expected) () ->
               let tracker = Origin_tracking.make () in
               let typ = Lang.Il.Typ.nat $ Common.Source.no_region in
               labeled_input "Offset_opt" (fun () ->
                   [
                     ("maybe", V.opt typ (Option.map nat content));
                     ("offset", nat 10);
                   ])
               |> evaluate_tracked ~name ~tracker mode
               |> rendered_origins tracker
               |> check_origins ~name:(name ^ ", Offset_opt")
                    ~mode_name:(fst mode) ~expected)
             [ (Some 1, [ "maybe"; "maybe?"; "offset" ]); (None, [ "maybe" ]) ])
       modes)

let run () =
  observers_preserve_results ();
  composed_observers_receive_events_in_order ();
  cache_hits_are_reported ();
  expression_events_reach_every_mode ();
  iteration_events_include_body_results ();
  observer_failure_finishes_handlers ();
  decoded_values_reach_observers ();
  decode_failures_finish_handlers ();
  updated_fields_keep_origins ();
  origins_follow_input_structure ();
  origins_combine_through_calls ();
  cache_hits_keep_current_origins ();
  repeated_runs_do_not_share_origins ();
  batch_test_cases_do_not_share_origins ();
  check_all
    [
      builtin_results_report_their_arguments;
      iteration_results_report_body_inputs;
    ]
