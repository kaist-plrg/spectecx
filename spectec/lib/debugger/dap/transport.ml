exception Protocol_error of string

let protocol_error fmt =
  Format.kasprintf (fun message -> raise (Protocol_error message)) fmt

type request = { seq : int; command : string; arguments : Yojson.Safe.t }
type t = { input : in_channel; output : out_channel; mutable next_seq : int }

let create input output =
  set_binary_mode_in input true;
  set_binary_mode_out output true;
  { input; output; next_seq = 1 }

(* Reading *)

let content_length = "content-length"

(* Reads header lines up to the blank line ending them, and returns the
   content length. [None] when the input ends before any header. *)
let read_headers input =
  let rec loop length ~started =
    match In_channel.input_line input with
    | None when not started -> None
    | None -> protocol_error "Input ended inside a message header."
    | Some line -> (
        let line =
          if String.ends_with ~suffix:"\r" line then
            String.sub line 0 (String.length line - 1)
          else line
        in
        if line = "" then
          match length with
          | Some length -> Some length
          | None -> protocol_error "Message header has no Content-Length."
        else
          match String.index_opt line ':' with
          | None -> protocol_error "Malformed header line `%s`." line
          | Some idx ->
              let name = String.sub line 0 idx |> String.trim in
              let value =
                String.sub line (idx + 1) (String.length line - idx - 1)
                |> String.trim
              in
              if String.lowercase_ascii name = content_length then
                match int_of_string_opt value with
                | Some length when length >= 0 ->
                    loop (Some length) ~started:true
                | _ -> protocol_error "Bad Content-Length `%s`." value
              else loop length ~started:true)
  in
  loop None ~started:false

let request_of_json json =
  let open Yojson.Safe.Util in
  let field name convert =
    match convert (member name json) with
    | value -> value
    | exception Type_error _ -> protocol_error "Message has no valid `%s`." name
  in
  (match field "type" to_string with
  | "request" -> ()
  | kind -> protocol_error "Expected a request, got a %s." kind);
  {
    seq = field "seq" to_int;
    command = field "command" to_string;
    arguments = member "arguments" json;
  }

let read_request conn =
  match read_headers conn.input with
  | None -> None
  | Some length ->
      let content =
        match really_input_string conn.input length with
        | content -> content
        | exception End_of_file ->
            protocol_error "Input ended inside a message body."
      in
      let json =
        match Yojson.Safe.from_string content with
        | json -> json
        | exception Yojson.Json_error message ->
            protocol_error "Message is not JSON: %s" message
      in
      Some (request_of_json json)

(* Writing *)

(* Numbers and writes one message. [fields] come after [seq]. *)
let send conn fields =
  let seq = conn.next_seq in
  conn.next_seq <- seq + 1;
  let content = Yojson.Safe.to_string (`Assoc (("seq", `Int seq) :: fields)) in
  Printf.fprintf conn.output "Content-Length: %d\r\n\r\n%s"
    (String.length content) content;
  flush conn.output

let with_body body fields =
  match body with Some body -> fields @ [ ("body", body) ] | None -> fields

let response request ~success =
  [
    ("type", `String "response");
    ("request_seq", `Int request.seq);
    ("success", `Bool success);
    ("command", `String request.command);
  ]

let respond conn ?body request =
  send conn (response request ~success:true |> with_body body)

let respond_error conn request message =
  send conn (response request ~success:false @ [ ("message", `String message) ])

let send_event conn ?body name =
  send conn
    ([ ("type", `String "event"); ("event", `String name) ] |> with_body body)
