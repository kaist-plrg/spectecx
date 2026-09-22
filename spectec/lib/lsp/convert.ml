open Common.Source
module Lsp = Linol_eio

type encoding = [ `UTF8 | `UTF16 ]

let line ~text number =
  if number < 0 then None
  else
    String.split_on_char '\n' text |> fun lines ->
    Option.map
      (fun line ->
        let length = String.length line in
        if length > 0 && line.[length - 1] = '\r' then
          String.sub line 0 (length - 1)
        else line)
      (List.nth_opt lines number)

let widths line byte =
  let decoded = String.get_utf_8_uchar line byte in
  let bytes = Uchar.utf_decode_length decoded in
  let units =
    if Uchar.to_int (Uchar.utf_decode_uchar decoded) > 0xffff then 2 else 1
  in
  (bytes, units)

let character ~encoding ~line column =
  let stop = max 0 (min column (String.length line)) in
  match encoding with
  | `UTF8 -> stop
  | `UTF16 ->
      let rec count byte units =
        if byte >= stop then units
        else
          let bytes, width = widths line byte in
          if byte + bytes > stop then units
          else count (byte + bytes) (units + width)
      in
      count 0 0

let position ~encoding ~text (pos : pos) =
  let number = max 0 (pos.line - 1) in
  let contents = Option.value (line ~text number) ~default:"" in
  Lsp.Position.create ~line:number
    ~character:(character ~encoding ~line:contents pos.column)

let range ~encoding ~text (region : region) =
  Lsp.Range.create
    ~start:(position ~encoding ~text region.left)
    ~end_:(position ~encoding ~text region.right)
