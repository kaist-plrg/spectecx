module Json = Yojson.Safe.Util

let request id method_ params =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("method", `String method_);
      ("params", params);
    ]

let notification method_ params =
  `Assoc
    [
      ("jsonrpc", `String "2.0"); ("method", `String method_); ("params", params);
    ]

let initialize = request 1 "initialize" (`Assoc [ ("capabilities", `Assoc []) ])
let initialized = notification "initialized" (`Assoc [])
let shutdown = request 2 "shutdown" `Null
let exit_ = notification "exit" `Null

let did_open uri text =
  notification "textDocument/didOpen"
    (`Assoc
       [
         ( "textDocument",
           `Assoc
             [
               ("uri", `String uri);
               ("languageId", `String "spectec");
               ("version", `Int 1);
               ("text", `String text);
             ] );
       ])

let did_change uri text version =
  notification "textDocument/didChange"
    (`Assoc
       [
         ( "textDocument",
           `Assoc [ ("uri", `String uri); ("version", `Int version) ] );
         ("contentChanges", `List [ `Assoc [ ("text", `String text) ] ]);
       ])

let did_close uri =
  notification "textDocument/didClose"
    (`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])

let send channel message =
  let body = Yojson.Safe.to_string message in
  Printf.fprintf channel "Content-Length: %d\r\n\r\n%s" (String.length body)
    body;
  flush channel

let receive channel =
  let rec content_length so_far =
    match String.trim (input_line channel) with
    | "" -> so_far
    | header -> (
        match String.split_on_char ':' header with
        | [ name; value ]
          when String.lowercase_ascii (String.trim name) = "content-length" ->
            content_length (int_of_string (String.trim value))
        | _ -> content_length so_far)
  in
  Yojson.Safe.from_string (really_input_string channel (content_length 0))

(* Close-on-exec so the server does not inherit our pipe ends; closing our write
   end then gives it EOF on stdin, which is what makes it exit. *)
let roundtrip server_bin messages =
  let from_server_r, from_server_w = Unix.pipe ~cloexec:true () in
  let to_server_r, to_server_w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process server_bin [| server_bin |] to_server_r from_server_w
      Unix.stderr
  in
  Unix.close to_server_r;
  Unix.close from_server_w;
  let from_server = Unix.in_channel_of_descr from_server_r in
  let to_server = Unix.out_channel_of_descr to_server_w in
  List.iter (send to_server) messages;
  close_out to_server;
  let rec collect replies =
    match receive from_server with
    | reply -> collect (reply :: replies)
    | exception End_of_file -> List.rev replies
  in
  let replies = collect [] in
  close_in from_server;
  let _, status = Unix.waitpid [] pid in
  assert (status = Unix.WEXITED 0);
  replies

let overlapping server_bin =
  let uri file =
    Linol_eio.DocumentUri.(
      to_string (of_path (Filename.concat (Sys.getcwd ()) file)))
  in
  let outer = uri "overlapping/1-outer.spectec"
  and inner = uri "overlapping/inner/2-inner.spectec" in
  let replies =
    roundtrip server_bin
      [
        initialize;
        initialized;
        did_open outer "syntax outer = nat\n";
        did_open inner "syntax inner = outer\n";
        did_change outer "syntax outer = text\n" 2;
        did_close outer;
        did_change inner "syntax inner = nat\n" 2;
        did_close inner;
        shutdown;
        exit_;
      ]
  in
  let counts =
    List.filter_map
      (fun reply ->
        if
          Json.member "method" reply
          <> `String "textDocument/publishDiagnostics"
        then None
        else
          let params = Json.member "params" reply in
          if Json.member "uri" params <> `String inner then None
          else
            Some (List.length (Json.to_list (Json.member "diagnostics" params))))
      replies
  in
  assert (counts = [ 0; 1; 1; 1; 0; 0 ]);
  print_endline "overlapping spec diagnostics: ok"

let () =
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle
       (fun _ ->
         prerr_endline "e2e: timed out";
         exit 2));
  ignore (Unix.alarm 10);
  let uri = "file:///e2e.spectec" in
  let file_uri path =
    Linol_eio.DocumentUri.of_path (Filename.concat (Sys.getcwd ()) path)
    |> Linol_eio.DocumentUri.to_string
  in
  let definition_uri = file_uri "multifile/1-def.spectec" in
  let use_uri = file_uri "multifile/2-use.spectec" in
  let replies =
    roundtrip Sys.argv.(1)
      [
        initialize;
        initialized;
        did_open uri "syntax t = foo\n";
        did_open definition_uri "syntax foo = nat\n";
        did_open use_uri "syntax t = fresh\n";
        did_change definition_uri "syntax foo = nat\nsyntax fresh = text\n" 2;
        did_change definition_uri "syntax foo =\n" 3;
        did_change definition_uri "syntax foo = nat\nsyntax fresh = text\n" 4;
        did_close definition_uri;
        did_close use_uri;
        shutdown;
        exit_;
      ]
  in
  let answered_initialize =
    List.exists
      (fun reply ->
        Json.member "id" reply = `Int 1 && Json.member "result" reply <> `Null)
      replies
  in
  Printf.printf "## end-to-end (spectecx-lsp over stdio)\n";
  Printf.printf "initialize: %s\n"
    (if answered_initialize then "ok" else "MISSING");
  List.iter
    (fun reply ->
      if Json.member "method" reply = `String "textDocument/publishDiagnostics"
      then
        let params = Json.member "params" reply in
        let uri = Json.to_string (Json.member "uri" params) in
        let uri =
          if uri = definition_uri then "definitions"
          else if uri = use_uri then "uses"
          else uri
        in
        let diagnostics = Json.to_list (Json.member "diagnostics" params) in
        let code diagnostic =
          match Json.member "code" diagnostic with `String s -> s | _ -> "?"
        in
        let codes = String.concat ", " (List.map code diagnostics) in
        Printf.printf "publishDiagnostics %s: %d diagnostic(s) [%s]\n" uri
          (List.length diagnostics) codes)
    replies;
  overlapping Sys.argv.(1)
