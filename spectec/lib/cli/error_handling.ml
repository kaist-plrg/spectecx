(* Resolve [--color] into an [Ansi.t]. [Auto] honors the [NO_COLOR] env
   variable convention and falls back on a stderr TTY check. *)
let resolve_ansi : Cli_args.color -> Spectec.Diagnostic.Ansi.t = function
  | Always -> Spectec.Diagnostic.Ansi.color
  | Never -> Spectec.Diagnostic.Ansi.plain
  | Auto ->
      if Sys.getenv_opt "NO_COLOR" = None && Unix.isatty Unix.stderr then
        Spectec.Diagnostic.Ansi.color
      else Spectec.Diagnostic.Ansi.plain

let guard ~color ~on_ok f =
  let ansi = resolve_ansi color in
  let result, bag = Spectec.with_diagnostics f in
  let combined =
    match result with
    | Ok _ -> bag
    | Error e ->
        Spectec.Diagnostic.Bag.merge bag (Spectec.Error.to_diagnostics e)
  in
  if not (Spectec.Diagnostic.Bag.is_empty combined) then
    Printf.eprintf "%s\n%!"
      (Spectec.Diagnostic.Render.render_bag ~ansi combined);
  match result with Ok v -> on_ok v | Error _ -> exit 1

let guard_unit ~color f = guard ~color ~on_ok:ignore f
