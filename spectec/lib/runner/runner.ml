open Il

module Handlers = struct
  let il f =
    let vid_counter = ref 0 in
    let tid_counter = ref 0 in
    Effect.Deep.try_with f ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | FreshVid ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let id = !vid_counter in
                    incr vid_counter;
                    Effect.Deep.continue k (fun () -> id))
            | FreshTid ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let tid = "FRESH__" ^ string_of_int !tid_counter in
                    incr tid_counter;
                    Effect.Deep.continue k (fun () -> tid))
            | ValueCreated _ ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    (* No-op *)
                    Effect.Deep.continue k ())
            | _ -> None (* Other effects *));
      }
end

let parse_file includes_target filename_target =
  Handlers.il (fun () -> P4.Parse.parse_file includes_target filename_target)

let parse_string filename_target string =
  Handlers.il (fun () -> P4.Parse.parse_string filename_target string)

let interp_il ~debug ~profile spec_il includes_target filename_target =
  Handlers.il (fun () ->
      let value_program = P4.Parse.parse_file includes_target filename_target in
      let ctx_init = Interp_il.Runner.init ~debug ~profile filename_target in
      Interp_il.Runner.run_relation ctx_init spec_il "Program_ok"
        [ value_program ])
