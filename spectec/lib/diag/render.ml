(** Diagnostic rendering — single rustc/elm-style layout with optional color.

    The renderer produces a sequence of independent sections (header, location
    + snippet, source tag, note, related, trace) and joins them with newlines.
      Each section is built by its own small function returning [string option],
      so adding or reordering a section is a one-line change. *)

open Common.Source

(* --- Severity styling --- *)

let severity_label : Record.severity -> string = function
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "info"
  | Hint -> "hint"

let severity_styles : Record.severity -> Ansi.style list = function
  | Error -> [ Bold; Red ]
  | Warning -> [ Bold; Yellow ]
  | Info -> [ Bold; Blue ]
  | Hint -> [ Bold; Cyan ]

(* --- Header --- *)

let render_header ~ansi (d : Record.t) : string =
  let label = severity_label d.severity in
  let code = match d.code with Some c -> "[" ^ c ^ "]" | None -> "" in
  let prefix =
    Ansi.style ansi (severity_styles d.severity) (label ^ code ^ ": ")
  in
  let message = Ansi.style ansi [ Bold ] d.message in
  prefix ^ message

(* --- Source snippet --- *)

(* TODO: multi-line regions currently render only the first line followed by
   "...". A future improvement would render every line in the span with the
   underline tracking the relevant columns on each row. *)
let render_snippet ~ansi ~cache ~(left : pos) ~(right : pos) : string option =
  match Source_cache.get_line cache left.file left.line with
  | None -> None
  | Some line_text ->
      let lineno_str = string_of_int left.line in
      let gutter = String.make (String.length lineno_str) ' ' in
      let same_line = left.line = right.line in
      let col_start = max 0 left.column in
      let col_end =
        if same_line then max (col_start + 1) right.column
        else max (col_start + 1) (String.length line_text)
      in
      let underline_len = max 1 (col_end - col_start) in
      (* Tab-aware indent: copy whitespace verbatim from the source line so the
         caret aligns under the right column whether the source uses spaces or
         tabs. Non-whitespace prefix chars collapse to a single space each. *)
      let indent =
        String.init col_start (fun i ->
            if i < String.length line_text && line_text.[i] = '\t' then '\t'
            else ' ')
      in
      let underline =
        indent ^ Ansi.style ansi [ Bold; Red ] (String.make underline_len '^')
      in
      let cont = if same_line then "" else "\n" ^ gutter ^ " | ..." in
      Some
        (Printf.sprintf "%s |\n%s | %s\n%s | %s%s" gutter lineno_str line_text
           gutter underline cont)

let render_location ~ansi ~cache (d : Record.t) : string option =
  if d.region = no_region then None
  else
    let arrow = Ansi.style ansi [ Bold; Blue ] "  --> " in
    let loc =
      Printf.sprintf "%s:%d:%d" d.region.left.file d.region.left.line
        (d.region.left.column + 1)
    in
    let head = arrow ^ loc in
    match
      render_snippet ~ansi ~cache ~left:d.region.left ~right:d.region.right
    with
    | None -> Some head
    | Some snippet -> Some (head ^ "\n" ^ snippet)

(* --- Annotations: source tag, note, related --- *)

let render_source_tag ~ansi (d : Record.t) : string option =
  if d.source = "" then None
  else Some (Ansi.style ansi [ Dim ] (Printf.sprintf "  = source: %s" d.source))

let render_detail ~ansi (d : Record.t) : string option =
  match d.detail with
  | None -> None
  | Some s -> Some (Ansi.style ansi [ Bold; Cyan ] "  = note: " ^ s)

let render_related ~ansi ~cache (d : Record.t) : string option =
  if d.related = [] then None
  else
    let one (r : Record.related) =
      let header = Ansi.style ansi [ Bold; Blue ] "  = related: " ^ r.message in
      if r.region = no_region then header
      else
        let arrow = Ansi.style ansi [ Bold; Blue ] "  --> " in
        let loc =
          Printf.sprintf "%s:%d:%d" r.region.left.file r.region.left.line
            (r.region.left.column + 1)
        in
        let loc_line = arrow ^ loc in
        match
          render_snippet ~ansi ~cache ~left:r.region.left ~right:r.region.right
        with
        | None -> String.concat "\n" [ header; loc_line ]
        | Some snippet -> String.concat "\n" [ header; loc_line; snippet ]
    in
    Some (List.map one d.related |> String.concat "\n")

(* --- Trace --- *)

let rec render_trace_node ~ansi ~indent ~is_last (node : Record.trace_node) :
    string =
  let { Record.region; message; children } = node in
  let connector = if is_last then "└── " else "├── " in
  let region_str =
    if region = no_region then ""
    else Ansi.style ansi [ Dim ] (string_of_region region) ^ " "
  in
  let line = indent ^ connector ^ region_str ^ message in
  let child_indent = indent ^ if is_last then "    " else "│   " in
  let n = List.length children in
  let child_lines =
    List.mapi
      (fun i c ->
        render_trace_node ~ansi ~indent:child_indent ~is_last:(i = n - 1) c)
      children
  in
  String.concat "\n" (line :: child_lines)

let render_trace ~ansi (d : Record.t) : string option =
  if d.trace = [] then None
  else
    let header = Ansi.style ansi [ Bold; Blue ] "  = trace:" in
    let n = List.length d.trace in
    let nodes =
      List.mapi
        (fun i node ->
          render_trace_node ~ansi ~indent:"    " ~is_last:(i = n - 1) node)
        d.trace
    in
    Some (String.concat "\n" (header :: nodes))

(* --- Public API --- *)

let render ~ansi ~cache (d : Record.t) : string =
  [
    Some (render_header ~ansi d);
    render_location ~ansi ~cache d;
    render_source_tag ~ansi d;
    render_detail ~ansi d;
    render_related ~ansi ~cache d;
    render_trace ~ansi d;
  ]
  |> List.filter_map Fun.id |> String.concat "\n"

let render_bag ~ansi bag =
  let cache = Source_cache.create () in
  Record.Bag.to_sorted_list bag
  |> List.map (render ~ansi ~cache)
  |> String.concat "\n"
