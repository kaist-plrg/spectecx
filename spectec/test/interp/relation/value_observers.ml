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

let run () =
  observers_preserve_results ();
  composed_observers_receive_events_in_order ();
  cache_hits_are_reported ();
  expression_events_reach_every_mode ();
  iteration_events_include_body_results ();
  observer_failure_finishes_handlers ()
