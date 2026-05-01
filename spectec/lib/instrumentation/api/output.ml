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
