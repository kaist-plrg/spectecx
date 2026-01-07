(* Output destination abstraction for instrumentation handlers.

   Provides a unified interface for writing to stdout or files.
   Each handler receives an output destination at creation time.

   Key features:
   - Lazy file opening: files are created only when first written
   - Automatic flushing: formatters are configured to flush on newline
   - Clean close: flushes and closes file channels
*)

type t =
  | Stdout
  | File of { path : string; mutable channel : out_channel option }

let stdout = Stdout
let file path = File { path; channel = None }

let formatter = function
  | Stdout -> Format.std_formatter
  | File f -> (
      match f.channel with
      | Some oc -> Format.formatter_of_out_channel oc
      | None ->
          let oc = open_out f.path in
          f.channel <- Some oc;
          let fmt = Format.formatter_of_out_channel oc in
          (* Enable auto-flush on newline for real-time output *)
          Format.pp_set_formatter_out_functions fmt
            {
              (Format.pp_get_formatter_out_functions fmt ()) with
              out_newline =
                (fun () ->
                  output_char oc '\n';
                  flush oc);
            };
          fmt)

let close = function
  | Stdout -> ()
  | File f -> (
      match f.channel with
      | Some oc ->
          flush oc;
          close_out oc;
          f.channel <- None
      | None -> ())
