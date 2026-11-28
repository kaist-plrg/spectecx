let frame () =
  In_channel.input_lines stdin
  |> List.filter (fun line -> String.trim line <> "")
  |> List.iter (fun content ->
         Printf.printf "Content-Length: %d\r\n\r\n%s" (String.length content)
           content)

let unframe () =
  let input = In_channel.input_all stdin in
  let separator = "\r\n\r\n" in
  let rec find i =
    if i + String.length separator > String.length input then None
    else if String.sub input i (String.length separator) = separator then Some i
    else find (i + 1)
  in
  let rec loop start =
    match find start with
    | None -> ()
    | Some header_end ->
        let header = String.sub input start (header_end - start) in
        let length = Scanf.sscanf header "Content-Length: %d" Fun.id in
        let body_start = header_end + String.length separator in
        print_endline (String.sub input body_start length);
        loop (body_start + length)
  in
  loop 0

let () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  match Sys.argv with
  | [| _; "frame" |] -> frame ()
  | [| _; "unframe" |] -> unframe ()
  | _ ->
      prerr_endline "usage: framing (frame | unframe)";
      exit 2
