(** The splice driver.

    [run] takes the elaborated [El.spec], the rendered [Pl.spec], and a list of
    [(input, output)] file pairs. For each input file it walks the file once,
    replacing every recognised [${name: ids}] anchor with rendered text, and
    writes the result to the output path. Unrecognised anchor names produce a
    diagnostic with file:line location. After all files are processed, [run]
    returns a {!Report.t} listing every key registered in a store but never
    referenced by any anchor. *)

type anchor = { frame : Anchor.frame; store : Store.t }

module StringMap = Map.Make (String)

let build_anchors ~spec_el ~spec_pl ~(source_entries : Anchor.Source.entry list)
    ~(prose_entries : Anchor.Prose.entry list) : anchor StringMap.t =
  let anchors = ref StringMap.empty in
  List.iter
    (fun (entry : Anchor.Source.entry) ->
      let pairs = entry.extract spec_el in
      let store = Store.create pairs in
      anchors :=
        StringMap.add entry.frame.name { frame = entry.frame; store } !anchors)
    source_entries;
  List.iter
    (fun (entry : Anchor.Prose.entry) ->
      let pairs = entry.extract spec_pl in
      let store = Store.create pairs in
      anchors :=
        StringMap.add entry.frame.name { frame = entry.frame; store } !anchors)
    prose_entries;
  !anchors

let render_anchor ({ frame; store } : anchor) ~(keys : string list)
    ~(location : Common.Location.t) : string =
  let resolved =
    List.filter_map
      (fun key ->
        match Store.find_opt store key with
        | Some text -> Some (key, text)
        | None ->
            Diag.warn location "splice"
              (Printf.sprintf "%s splice key not found: %s" frame.name key);
            None)
      keys
  in
  let headers =
    if frame.header then
      let unused_keys =
        List.filter_map
          (fun (key, _) ->
            if Store.is_used store key then None else Some ("[[" ^ key ^ "]]"))
          resolved
      in
      match unused_keys with [] -> "" | hs -> String.concat "\n" hs ^ "\n"
    else ""
  in
  List.iter (fun (key, _) -> Store.mark_used store key) resolved;
  let body = resolved |> List.map snd |> String.concat "\n\n" in
  headers ^ frame.prefix ^ body ^ frame.suffix

let splice_string ~(anchors : anchor StringMap.t) ~file (content : string) :
    string =
  let cur = Cursor.make ~file content in
  let buf = Buffer.create (String.length content) in
  let rec loop () =
    if Cursor.eos cur then ()
    else (
      (match Parser.parse_anchor_open cur with
      | Some (name, location) -> (
          match StringMap.find_opt name anchors with
          | Some anchor ->
              Parser.parse_space cur;
              let keys = Parser.parse_ids cur in
              Buffer.add_string buf (render_anchor anchor ~keys ~location)
          | None ->
              Diag.warn location "splice"
                (Printf.sprintf "unknown splice anchor: %s" name);
              Buffer.add_string buf ("${" ^ name ^ ":"))
      | None ->
          Buffer.add_char buf (Cursor.peek cur);
          Cursor.adv cur);
      loop ())
  in
  loop ();
  Buffer.contents buf

let gen_directory (filename : string) : unit =
  let rec gen dir =
    if not (Sys.file_exists dir) then (
      let parent = Filename.dirname dir in
      if parent <> dir then gen parent;
      Unix.mkdir dir 0o755)
  in
  let dirname = Filename.dirname filename in
  if dirname <> "" && not (Sys.file_exists dirname) then gen dirname

let splice_file ~(anchors : anchor StringMap.t) (filename_in : string)
    (filename_out : string) : unit =
  let ic = open_in filename_in in
  let content =
    Fun.protect
      (fun () -> In_channel.input_all ic)
      ~finally:(fun () -> In_channel.close ic)
  in
  let spliced = splice_string ~anchors ~file:filename_in content in
  gen_directory filename_out;
  let oc = open_out filename_out in
  Fun.protect
    (fun () -> Out_channel.output_string oc spliced)
    ~finally:(fun () -> Out_channel.close oc)

let run ~spec_el ~spec_pl ~(source_entries : Anchor.Source.entry list)
    ~(prose_entries : Anchor.Prose.entry list)
    ~(filenames : (string * string) list) : Report.t =
  let anchors =
    build_anchors ~spec_el ~spec_pl ~source_entries ~prose_entries
  in
  List.iter (fun (i, o) -> splice_file ~anchors i o) filenames;
  anchors |> StringMap.bindings
  |> List.map (fun (name, anchor) -> (name, anchor.store))
  |> Report.of_stores
