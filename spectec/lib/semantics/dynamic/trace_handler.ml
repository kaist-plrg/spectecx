(* Trace handler - Live logging of relation/function calls.

   Implements Instr_hooks.HANDLER interface.
   Prints indented call tree with enter/exit markers.

   Usage:
     let handler = Trace_handler.make () in
     Instr_hooks.run_with_handlers ~handlers_list:[handler] (fun () -> ...)
*)

module State = struct
  let depth = ref 0
  let reset () = depth := 0

  let indent () =
    Format.sprintf "[%2d] %s" !depth (String.make (!depth * 2) ' ')
end

module Handler : Instr_hooks.HANDLER = struct
  let on_rel_enter ~id ~at:_ ~values:_ =
    Format.printf "%s→ %s\n%!" (State.indent ()) id;
    incr State.depth

  let on_rel_exit ~id ~at:_ ~success =
    decr State.depth;
    Format.printf "%s← %s [%s]\n%!" (State.indent ()) id
      (if success then "ok" else "fail")

  let on_func_enter ~id ~at:_ ~values:_ =
    Format.printf "%s→ $%s\n%!" (State.indent ()) id;
    incr State.depth

  let on_func_exit ~id ~at:_ =
    decr State.depth;
    Format.printf "%s← $%s\n%!" (State.indent ()) id

  let on_prem ~at:_ = ()
  let on_instr ~at:_ = ()
  let finish () = ()
end

let make () : (module Instr_hooks.HANDLER) =
  State.reset ();
  (module Handler)
