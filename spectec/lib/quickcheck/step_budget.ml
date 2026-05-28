module Event = Instrumentation_api.Event
module Handler = Instrumentation_api.Handler

exception StepLimitExceeded

let make ?max_steps () : (module Handler.S) =
  let budget = ref max_steps in
  (module struct
    let static_dependencies = []
    let init ~spec:_ = ()

    let handle (ev : Event.t) =
      match ev with
      | Event.Rel_enter _ -> (
          match !budget with
          | None -> ()
          | Some 0 -> raise StepLimitExceeded
          | Some n -> budget := Some (n - 1))
      | _ -> ()

    let finish () = ()
  end)

let with_budget ?max_steps spec f =
  let handler = make ?max_steps () in
  let static_spec = Handler.IlSpec spec in
  Instrumentation.Dispatcher.with_handler ~spec:static_spec handler f
