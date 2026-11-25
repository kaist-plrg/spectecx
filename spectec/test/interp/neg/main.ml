(** [main.exe SPEC_DIR FILE] typechecks [FILE] with the AL interpreter. It
    prints plain diagnostics to stderr and exits 1 if any were emitted.
    [SPEC_DIR] can select the impty [closure] spec. *)

module D = Spectec.Diagnostic

let ( let* ) = Result.bind

let run spec_dir file =
  let spec_files = Test_lib.Files.collect ~suffix:".spectec" spec_dir in
  let* spec = Spectec.parse_spec_files spec_files in
  let* _spec_il, spec_al = Spectec.elaborate_and_algorithmize spec in
  let input =
    { Targets_impty.Impty.filename = file; expect = Spectec.Task.Negative }
  in
  Spectec.eval_task_with_instrumentation
    (module Targets_impty.Impty.Typecheck)
    ~mode:Spectec.Interp_mode.Al ~spec_al input
  |> Result.map ignore

let () =
  let spec_dir = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  let _, bag = Spectec.with_diagnostics (fun () -> run spec_dir file) in
  prerr_string (D.Render.render_bag ~ansi:D.Ansi.plain bag);
  if D.Bag.is_empty bag then exit 0 else exit 1
