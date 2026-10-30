type pos = { line : int; column : int }

type t =
  | Unknown
  | File of string
  | Span of { file : string; left : pos; right : pos }

let of_lexing ((left, right) : Lexing.position * Lexing.position) =
  let pos (p : Lexing.position) =
    { line = p.pos_lnum; column = p.pos_cnum - p.pos_bol }
  in
  Span { file = left.pos_fname; left = pos left; right = pos right }

let file = function
  | Unknown -> None
  | File file | Span { file; _ } -> Some file

let cover locations =
  let join location_l location_r =
    match (location_l, location_r) with
    | Unknown, location | location, Unknown -> location
    | _ when file location_l <> file location_r ->
        invalid_arg "Location.cover: locations in two files"
    | (File _ as location), _ | _, (File _ as location) -> location
    | Span span_l, Span span_r ->
        Span
          {
            file = span_l.file;
            left = min span_l.left span_r.left;
            right = max span_l.right span_r.right;
          }
  in
  List.fold_left join Unknown locations

let compare location_l location_r =
  let key = function
    | Unknown -> (0, "", None)
    | File file -> (1, file, None)
    | Span { file; left; _ } -> (1, file, Some left)
  in
  Stdlib.compare (key location_l) (key location_r)

let string_of_range left right =
  let string_of_pos { line; column } =
    Printf.sprintf "%d:%d" line (column + 1)
  in
  if left = right then string_of_pos left
  else string_of_pos left ^ "-" ^ string_of_pos right

let to_string = function
  | Unknown -> "<unknown>"
  | File file -> file
  | Span { file; left; right } -> file ^ ":" ^ string_of_range left right
