type session = Idle | Active of (module Instrumentation_api.Handler.S) list

let session = ref Idle

let init ~spec ~handlers =
  (match !session with
  | Idle -> ()
  | Active _ ->
      failwith "Instrumentation.Dispatcher.init: instrumentation already active");
  List.iter
    (fun (module H : Instrumentation_api.Handler.S) -> H.init ~spec)
    handlers;
  session := Active handlers

let emit (ev : Instrumentation_api.Event.t) : unit =
  match !session with
  | Active hs ->
      List.iter
        (fun (module H : Instrumentation_api.Handler.S) -> H.handle ev)
        hs
  | Idle -> ()

let finish () =
  match !session with
  | Idle -> ()
  | Active hs ->
      List.iter
        (fun (module H : Instrumentation_api.Handler.S) -> H.finish ())
        hs;
      session := Idle
