type session = Idle | Active of (module Handler.S) list

let session = ref Idle

let init ~spec ~handlers =
  (match !session with
  | Idle -> ()
  | Active _ ->
      failwith "Instrumentation.Dispatcher.init: session already active");
  List.iter (fun (module H : Handler.S) -> H.init ~spec) handlers;
  session := Active handlers

let emit (ev : Handler.event) : unit =
  match !session with
  | Active hs -> List.iter (fun (module H : Handler.S) -> H.handle ev) hs
  | Idle -> ()

let finish () =
  match !session with
  | Idle -> ()
  | Active hs ->
      List.iter (fun (module H : Handler.S) -> H.finish ()) hs;
      session := Idle
