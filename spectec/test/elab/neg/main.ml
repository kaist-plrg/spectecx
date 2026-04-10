(** Elaboration negative test harness.

    Invoked once per [.spectec] input. Runs parse + elaborate, renders any
    diagnostics (errors and warnings) to stderr in plain (non-ANSI) form, and
    exits with status 1 if any diagnostic was emitted. *)

let () =
  let file = Sys.argv.(1) in
  let result, bag =
    Spectec.with_diagnostics (fun () ->
        match Spectec.parse_spec_files [ file ] with
        | Error e -> Error e
        | Ok spec_el -> Spectec.elaborate spec_el)
  in
  let combined =
    match result with
    | Ok _ -> bag
    | Error e ->
        Spectec.Diagnostic.Bag.merge bag (Spectec.Error.to_diagnostics e)
  in
  let rendered =
    Spectec.Diagnostic.Render.render_bag ~ansi:Spectec.Diagnostic.Ansi.plain
      combined
  in
  prerr_string rendered;
  if Spectec.Diagnostic.Bag.is_empty combined then exit 0 else exit 1
