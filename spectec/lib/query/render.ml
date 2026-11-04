module Location = Common.Location
module Group = Common.Group

type line_range = { file : string; first_line : int }

let line_range_of_location (location : Location.t) =
  match location with
  | Span { file; left; _ } -> Some { file; first_line = left.line }
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
