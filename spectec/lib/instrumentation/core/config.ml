type t = {
  name : string;
  mode : [ `IL | `SL | `Both ];
  handler : (module Handler.S);
  output : Output.t;
}

let register_static_dependencies ({ handler; _ } : t) =
  let module H = (val handler : Handler.S) in
  List.iter
    (fun (module M : Instrumentation_static.Static.S) ->
      Instrumentation_static.Static.register (module M))
    H.static_dependencies

let to_handler ({ handler; _ } : t) = handler
let close_output ({ output; _ } : t) = Output.close output
