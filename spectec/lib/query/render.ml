module Location = Common.Location
module Group = Common.Group

type line_range = { file : string; first_line : int; last_line : int }

let line_range_of_location (location : Location.t) =
  match location with
  | Span { file; left; right } ->
      Some { file; first_line = left.line; last_line = right.line }
  | Unknown | File _ -> None

let pair_with_line_ranges items ~location_of =
  List.filter_map
    (fun item ->
      Option.map
        (fun line_range -> (item, line_range))
        (line_range_of_location (location_of item)))
    items

let string_of_count n ~noun =
  Printf.sprintf "%d %s%s" n noun (if n = 1 then "" else "s")

let label_of_definition (definition : Definitions.definition) =
  Definitions.string_of_kind definition.kind ^ " " ^ definition.name

let string_of_line_range { first_line; last_line; _ } =
  if first_line = last_line then string_of_int first_line
  else Printf.sprintf "%d-%d" first_line last_line

let string_of_lines lines =
  String.concat "" (List.map (fun line -> line ^ "\n") lines)

let render_uses uses =
  let use_positions =
    pair_with_line_ranges uses ~location_of:(fun (use : Uses.use) ->
        use.location)
    |> List.map (fun ((use : Uses.use), line_range) ->
           ( line_range.file,
             (label_of_definition use.owner, line_range.first_line) ))
  in
  let by_file = Group.in_order use_positions in
  let owners = Group.in_order (List.map snd use_positions) in
  let owner_row (label, line_numbers) =
    Printf.sprintf "  %s: %s" label
      (String.concat ", "
         (List.map string_of_int (List.sort_uniq Int.compare line_numbers)))
  in
  let summary =
    Printf.sprintf "%s in %s across %s"
      (string_of_count (List.length use_positions) ~noun:"use")
      (string_of_count (List.length owners) ~noun:"definition")
      (string_of_count (List.length by_file) ~noun:"file")
  in
  string_of_lines
    (List.concat_map
       (fun (file, owners_in_file) ->
         file :: List.map owner_row (Group.in_order owners_in_file))
       by_file
    @ [ summary ])

let render_definitions ~text_of_file definitions =
  pair_with_line_ranges definitions
    ~location_of:(fun (definition : Definitions.definition) ->
      definition.location)
  |> List.map (fun (_, line_range) ->
         let source_lines =
           Option.fold ~none:[]
             ~some:(String.split_on_char '\n')
             (text_of_file line_range.file)
           |> List.filteri (fun i _ ->
                  line_range.first_line <= i + 1
                  && i + 1 <= line_range.last_line)
         in
         string_of_lines
           (Printf.sprintf "%s:%s" line_range.file
              (string_of_line_range line_range)
           :: source_lines))
  |> String.concat "\n"

let render_outline definitions =
  let is_next_clause ((head : Definitions.definition), head_range)
      ((definition : Definitions.definition), line_range) =
    head.kind = Def && definition.kind = Def
    && String.equal head.name definition.name
    && String.equal head_range.file line_range.file
  in
  let rows =
    List.fold_left
      (fun rows ((_, line_range) as entry) ->
        match rows with
        | (head, clause_count, _) :: rows when is_next_clause head entry ->
            (head, clause_count + 1, line_range.last_line) :: rows
        | rows -> (entry, 1, line_range.last_line) :: rows)
      []
      (pair_with_line_ranges definitions
         ~location_of:(fun (definition : Definitions.definition) ->
           definition.location))
    |> List.rev
  in
  let row_line ((head, head_range), clause_count, last_line) =
    Printf.sprintf "  %s %s%s"
      (string_of_line_range { head_range with last_line })
      (label_of_definition head)
      (if clause_count > 1 then Printf.sprintf " (%d clauses)" clause_count
       else "")
  in
  List.map (fun (((_, head_range), _, _) as row) -> (head_range.file, row)) rows
  |> Group.in_order
  |> List.concat_map (fun (file, rows) -> file :: List.map row_line rows)
  |> string_of_lines

let diagnostics_of_missing_names names =
  List.map
    (fun name ->
      Diag.error ~source:"query" Location.Unknown ("no definition named " ^ name))
    names
  |> Diag.Bag.of_list
