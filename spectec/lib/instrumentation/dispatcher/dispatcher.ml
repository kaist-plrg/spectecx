module Exn = Instrumentation_common.Exn

type session =
  | Idle
  | Active of {
      handlers : (module Instrumentation_api.Handler.S) list;
      value_observers : (Instrumentation_api.Value_event.t -> unit) list;
    }

let session = ref Idle

let finish_all_handlers handlers =
  handlers
  |> List.fold_left
       (fun first_error -> function
         | (module H : Instrumentation_api.Handler.S) ->
             Exn.try_record_first_error first_error H.finish)
       None
  |> Exn.raise_recorded_error

let rec init_handlers ~spec initialized = function
  | [] -> ()
  | ((module H : Instrumentation_api.Handler.S) as handler) :: rest -> (
      try
        H.init ~spec;
        init_handlers ~spec (handler :: initialized) rest
      with exn ->
        let captured = Exn.capture exn in
        (try finish_all_handlers initialized with _ -> ());
        Exn.raise_captured captured)

let init ~spec ~handlers =
  (match !session with
  | Idle -> ()
  | Active _ ->
      failwith "Instrumentation.Dispatcher.init: instrumentation already active");
  init_handlers ~spec [] handlers;
  let value_observers =
    List.filter_map
      (fun (module H : Instrumentation_api.Handler.S) -> H.observe_value)
      handlers
  in
  session := Active { handlers; value_observers }

let dispatch hs (ev : Instrumentation_api.Event.t) : unit =
  List.iter (fun (module H : Instrumentation_api.Handler.S) -> H.handle ev) hs

let emit (ev : Instrumentation_api.Event.t) : unit =
  match !session with
  | Active { handlers; _ } -> dispatch handlers ev
  | Idle -> ()

let emit_on_demand (make : unit -> Instrumentation_api.Event.t) : unit =
  match !session with
  | Active { handlers; _ } -> dispatch handlers (make ())
  | Idle -> ()

let observes_values () =
  match !session with
  | Active { value_observers = _ :: _; _ } -> true
  | Active { value_observers = []; _ } | Idle -> false

let emit_value (ev : Instrumentation_api.Value_event.t) : unit =
  match !session with
  | Active { value_observers; _ } ->
      List.iter (fun observe -> observe ev) value_observers
  | Idle -> ()

let finish () =
  match !session with
  | Idle -> ()
  | Active { handlers; _ } ->
      session := Idle;
      finish_all_handlers handlers
