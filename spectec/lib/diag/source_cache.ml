type t = (string, string array option) Hashtbl.t

let create () : t = Hashtbl.create 16

let read_file file : string array option =
  try
    let ic = open_in file in
    let buf = Buffer.create 4096 in
    (try
       while true do
         Buffer.add_channel buf ic 4096
       done
     with End_of_file -> ());
    close_in ic;
    let lines = String.split_on_char '\n' (Buffer.contents buf) in
    (* strip the phantom empty trailing entry produced by a final newline *)
    let lines =
      match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
    in
    Some (Array.of_list lines)
  with Sys_error _ -> None

let load cache file =
  match Hashtbl.find_opt cache file with
  | Some entry -> entry
  | None ->
      let entry = read_file file in
      Hashtbl.add cache file entry;
      entry

let get_line cache file lineno =
  if file = "" || lineno < 1 then None
  else
    match load cache file with
    | None -> None
    | Some lines ->
        if lineno > Array.length lines then None else Some lines.(lineno - 1)
