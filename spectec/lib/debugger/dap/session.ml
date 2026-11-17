module Location = Common.Location

type t = {
  conn : Transport.t;
  mutable line_base : int;
  mutable column_base : int;
}

let current_session : t option ref = ref None

let current () =
  match !current_session with
  | Some session -> session
  | None -> invalid_arg "Dap.Session: not connected"

let connect () =
  Format.pp_print_flush Format.std_formatter ();
  let protocol = Unix.dup Unix.stdout in
  Unix.dup2 Unix.stderr Unix.stdout;
  let conn = Transport.create stdin (Unix.out_channel_of_descr protocol) in
  current_session := Some { conn; line_base = 1; column_base = 1 }

let thread_id = 1

let client_line session (pos : Location.pos) =
  `Int (pos.line - 1 + session.line_base)

let client_column session (pos : Location.pos) =
  `Int (pos.column + session.column_base)

let engine_line session line = line - session.line_base + 1
