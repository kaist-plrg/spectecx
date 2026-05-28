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

(* Backticks must pair; an unmatched one disables inline-code styling. *)
let style_inline_code ~ansi ~outer (s : string) : string =
  let parts = String.split_on_char '`' s in
  if List.length parts mod 2 = 0 then Ansi.style ansi outer s
  else
    List.mapi
      (fun i p ->
        if i mod 2 = 0 then Ansi.style ansi outer p
        else Ansi.style ansi [ Bold; Cyan ] ("`" ^ p ^ "`"))
      parts
    |> String.concat ""

(* --- Header --- *)

let render_header ~ansi (d : Record.t) : string =
  let label = severity_label d.severity in
  let code = match d.code with Some c -> "[" ^ c ^ "]" | None -> "" in
  let prefix =
    Ansi.style ansi (severity_styles d.severity) (label ^ code ^ ": ")
  in
  let message = style_inline_code ~ansi ~outer:[ Bold ] d.message in
  prefix ^ message

(* --- Source snippet --- *)

(* TODO: multi-line regions currently render only the first line followed by
   "...". A future improvement would render every line in the span with the
   underline tracking the relevant columns on each row. *)
let render_snippet ~ansi ~cache ~indent ~(left : pos) ~(right : pos) :
    string option =
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
      (* Copy tabs from the source verbatim so the caret aligns under the
         right column regardless of indentation style. *)
      let caret_pad =
        String.init col_start (fun i ->
            if i < String.length line_text && line_text.[i] = '\t' then '\t'
            else ' ')
      in
      let underline =
        caret_pad
        ^ Ansi.style ansi [ Bold; Red ] (String.make underline_len '^')
      in
      let cont = if same_line then "" else "\n" ^ indent ^ gutter ^ " | ..." in
      Some
        (Printf.sprintf "%s%s |\n%s%s | %s\n%s%s | %s%s" indent gutter indent
           lineno_str line_text indent gutter underline cont)

let render_region_block ~ansi ~cache ~indent (region : region) : string =
  let arrow = indent ^ Ansi.style ansi [ Bold; Blue ] "  --> " in
  let loc =
    Printf.sprintf "%s:%d:%d" region.left.file region.left.line
      (region.left.column + 1)
  in
  let arrow_line = arrow ^ loc in
  match
    render_snippet ~ansi ~cache ~indent ~left:region.left ~right:region.right
  with
  | None -> arrow_line
  | Some snippet -> arrow_line ^ "\n" ^ snippet

let render_location ~ansi ~cache (d : Record.t) : string option =
  if d.region = no_region then None
  else Some (render_region_block ~ansi ~cache ~indent:"" d.region)

(* --- Annotations: source tag, note, related --- *)

let snippet_gutter (d : Record.t) : string =
  if d.region = no_region then ""
  else String.make (String.length (string_of_int d.region.left.line)) ' '

(* [|] prefix in the same column as the snippet's border, so annotation
   fields hang off the snippet rather than float beside it. *)
let border_prefix (d : Record.t) : string =
  if d.region = no_region then "  " else " " ^ snippet_gutter d ^ "| "

(* Code-bearing diagnostics already name the pass via [code]. *)
let show_source (d : Record.t) : bool = d.source <> "" && d.code = None

let render_field_separator (d : Record.t) : string option =
  let has_field =
    show_source d || d.detail <> None || d.related <> [] || d.trace <> []
  in
  if d.region = no_region || not has_field then None
  else Some (" " ^ snippet_gutter d ^ "|")

let render_source_tag ~ansi (d : Record.t) : string option =
  if not (show_source d) then None
  else
    Some
      (border_prefix d
      ^ Ansi.style ansi [ Dim ] (Printf.sprintf "source: %s" d.source))

let split_words (s : string) : string list =
  let words = ref [] in
  let buf = Buffer.create 16 in
  let in_code = ref false in
  let flush () =
    if Buffer.length buf > 0 then (
      words := Buffer.contents buf :: !words;
      Buffer.clear buf)
  in
  String.iter
    (fun c ->
      match c with
      | '`' ->
          in_code := not !in_code;
          Buffer.add_char buf c
      | (' ' | '\t') when not !in_code -> flush ()
      | _ -> Buffer.add_char buf c)
    s;
  flush ();
  List.rev !words

(* Caller emits the head; we emit [tail_prefix] before each continuation line. *)
let wrap_prose ~ansi ~max_width ~head_width ~tail_prefix (s : string) : string =
  let style word =
    let n = String.length word in
    if n >= 2 && word.[0] = '`' && word.[n - 1] = '`' then
      Ansi.style ansi [ Bold; Cyan ] word
    else word
  in
  let render_line words = String.concat " " (List.rev_map style words) in
  let head_budget = max 1 (max_width - head_width) in
  let tail_budget = max 1 (max_width - String.length tail_prefix) in
  let rec fill_one_line budget line line_width = function
    | [] -> (line, [])
    | word :: rest as remaining ->
        let word_width = String.length word in
        let with_word =
          if line = [] then word_width else line_width + 1 + word_width
        in
        if line <> [] && with_word > budget then (line, remaining)
        else fill_one_line budget (word :: line) with_word rest
  in
  let rec produce_lines budget lines remaining =
    let line, rest = fill_one_line budget [] 0 remaining in
    let rendered = render_line line in
    if rest = [] then List.rev (rendered :: lines)
    else produce_lines tail_budget (rendered :: lines) rest
  in
  produce_lines head_budget [] (split_words s)
  |> String.concat ("\n" ^ tail_prefix)

let render_detail ~ansi (d : Record.t) : string option =
  match d.detail with
  | None -> None
  | Some s ->
      let prefix = border_prefix d in
      let label = "note: " in
      let tail_prefix = prefix ^ String.make (String.length label) ' ' in
      let wrapped =
        wrap_prose ~ansi ~max_width:80
          ~head_width:(String.length tail_prefix)
          ~tail_prefix s
      in
      Some (prefix ^ Ansi.style ansi [ Bold; Cyan ] label ^ wrapped)

let render_related ~ansi ~cache (d : Record.t) : string option =
  if d.related = [] then None
  else
    let one (r : Record.related) =
      let header =
        border_prefix d
        ^ Ansi.style ansi [ Bold; Blue ] "related: "
        ^ style_inline_code ~ansi ~outer:[] r.message
      in
      if r.region = no_region then header
      else
        header ^ "\n"
        ^ render_region_block ~ansi ~cache ~indent:(border_prefix d) r.region
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
  let line =
    indent ^ connector ^ region_str ^ style_inline_code ~ansi ~outer:[] message
  in
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
    let header = border_prefix d ^ Ansi.style ansi [ Bold; Blue ] "trace:" in
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
    render_field_separator d;
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
