(** Lexers for splice anchors.

    The driver lexes [${name:] once to obtain the anchor name, then dispatches
    by name. After dispatch, [parse_ids] consumes the keys list and the closing
    [}]. *)

let parse_space (cur : Cursor.t) : unit =
  while
    (not (Cursor.eos cur))
    &&
    let c = Cursor.peek cur in
    c = ' ' || c = '\t' || c = '\n'
  do
    Cursor.adv cur
  done

let is_id_char = function
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '0' .. '9'
  | '_' | '\'' | '`' | '-' | '*' | '.' | '/' ->
      true
  | _ -> false

let parse_id (cur : Cursor.t) : string option =
  let start = cur.i in
  while (not (Cursor.eos cur)) && is_id_char (Cursor.peek cur) do
    Cursor.adv cur
  done;
  if cur.i = start then None else Some (String.sub cur.s start (cur.i - start))

let parse_anchor_open (cur : Cursor.t) : (string * Common.Location.t) option =
  if not (Cursor.starts_with cur "${") then None
  else
    let i_save = cur.i in
    Cursor.adv cur;
    Cursor.adv cur;
    let left = Cursor.pos cur in
    match parse_id cur with
    | None ->
        cur.i <- i_save;
        None
    | Some name ->
        let right = Cursor.pos cur in
        if Cursor.consume cur ":" then
          Some (name, Common.Location.Span { file = cur.file; left; right })
        else (
          cur.i <- i_save;
          None)

let rec parse_ids (cur : Cursor.t) : string list =
  parse_space cur;
  if Cursor.consume cur "}" then []
  else match parse_id cur with None -> [] | Some id -> id :: parse_ids cur
