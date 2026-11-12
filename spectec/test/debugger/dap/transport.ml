(* Feeds framed input to Transport and prints what it reads, and prints the
   messages it writes, each as its header (with \r and \n spelled out) and
   its body. *)

module Transport = Spectec_debugger.Dap.Transport

let frame content =
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length content) content

(* Runs [f] on a connection reading [input], and returns what it wrote. *)
let with_conn input f =
  let in_file = Filename.temp_file "dap_in" "" in
  let out_file = Filename.temp_file "dap_out" "" in
  Out_channel.with_open_bin in_file (fun oc -> output_string oc input);
  let ic = open_in_bin in_file in
  let oc = open_out_bin out_file in
  Fun.protect
    ~finally:(fun () ->
      close_in ic;
      close_out_noerr oc;
      Sys.remove in_file;
      Sys.remove out_file)
    (fun () ->
      f (Transport.create ic oc);
      close_out oc;
      In_channel.with_open_bin out_file In_channel.input_all)

(* Reading *)

let print_request (request : Transport.request) =
  `Assoc
    [
      ("seq", `Int request.seq);
      ("command", `String request.command);
      ("arguments", request.arguments);
    ]
  |> Yojson.Safe.to_string |> print_endline

(* Reads requests until the input ends or a protocol error. *)
let read_all conn =
  let rec loop () =
    match Transport.read_request conn with
    | Some request ->
        print_request request;
        loop ()
    | None -> print_endline "(end of input)"
    | exception Transport.Protocol_error message ->
        Printf.printf "protocol error: %s\n" message
  in
  loop ()

let read_case name input =
  Printf.printf "## %s\n" name;
  ignore (with_conn input read_all);
  print_newline ()

(* Writing *)

let escape s =
  String.concat "\\r" (String.split_on_char '\r' s)
  |> String.split_on_char '\n' |> String.concat "\\n"

(* Splits written bytes into messages, printing each header and body. *)
let print_written written =
  let separator = "\r\n\r\n" in
  let rec loop start =
    if start < String.length written then (
      let rec find i =
        if String.sub written i (String.length separator) = separator then i
        else find (i + 1)
      in
      let header_end = find start + String.length separator in
      let header = String.sub written start (header_end - start) in
      let length = Scanf.sscanf header "Content-Length: %d" Fun.id in
      print_endline (escape header);
      print_endline (String.sub written header_end length);
      loop (header_end + length))
  in
  loop 0

let write_case name f =
  Printf.printf "## %s\n" name;
  print_written (with_conn "" f);
  print_newline ()

let request : Transport.request =
  { seq = 7; command = "stackTrace"; arguments = `Null }

let () =
  read_case "two requests, with and without arguments"
    (frame
       {|{"seq":1,"type":"request","command":"initialize","arguments":{"adapterID":"spectec"}}|}
    ^ frame {|{"seq":2,"type":"request","command":"threads"}|});
  read_case "empty input" "";
  (let content = {|{"seq":3,"type":"request","command":"configurationDone"}|} in
   read_case "other headers, LF line endings and any header case"
     (Printf.sprintf "Content-Type: application/json\ncontent-length: %d\n\n%s"
        (String.length content) content));
  read_case "length counts bytes, not characters"
    (frame
       {|{"seq":4,"type":"request","command":"evaluate","arguments":{"expression":"λx"}}|});
  read_case "missing Content-Length" "Content-Type: x\r\n\r\n{}";
  read_case "bad Content-Length" "Content-Length: -3\r\n\r\n";
  read_case "malformed header" "Content-Length 12\r\n\r\n";
  read_case "input ends in the header" "Content-Length: 12\r\n";
  read_case "input ends in the body" "Content-Length: 50\r\n\r\n{\"seq\":1}";
  read_case "not JSON" (frame "{seq:");
  read_case "not a request"
    (frame {|{"seq":1,"type":"response","command":"x"}|});
  read_case "request without a command" (frame {|{"seq":1,"type":"request"}|});
  write_case "respond" (fun conn -> Transport.respond conn request);
  write_case "respond with a body" (fun conn ->
      Transport.respond conn request ~body:(`Assoc [ ("totalFrames", `Int 0) ]));
  write_case "respond_error" (fun conn ->
      Transport.respond_error conn request "No call in progress.");
  write_case "send_event" (fun conn -> Transport.send_event conn "initialized");
  write_case "send_event with a body" (fun conn ->
      Transport.send_event conn "stopped"
        ~body:(`Assoc [ ("reason", `String "breakpoint") ]));
  write_case "seq counts each message on a connection" (fun conn ->
      Transport.respond conn request;
      Transport.send_event conn "initialized";
      Transport.send_event conn "terminated")
