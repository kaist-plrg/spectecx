module Convert = Spectec_lsp.Convert
module Lsp = Linol_eio

let () =
  let line = "é😀 e" in
  assert (Convert.character ~encoding:`UTF16 ~line 7 = 4);
  assert (Convert.character ~encoding:`UTF8 ~line 7 = 7);
  assert (Convert.character ~encoding:`UTF16 ~line 4 = 1);
  assert (Convert.character ~encoding:`UTF16 ~line 99 = 5);
  assert (Convert.character ~encoding:`UTF16 ~line (-1) = 0);
  assert (Convert.line ~text:"a\r\nb" 0 = Some "a");
  assert (Convert.line ~text:line (-1) = None);
  assert (Convert.line ~text:line 1 = None);
  let left = Common.Source.{ file = "unicode.spectec"; line = 1; column = 7 } in
  let right = { left with column = 8 } in
  List.iter
    (fun (encoding, start, stop) ->
      let range = Convert.range ~encoding ~text:line { left; right } in
      assert (range.start = Lsp.Position.create ~line:0 ~character:start);
      assert (range.end_ = Lsp.Position.create ~line:0 ~character:stop))
    [ (`UTF8, 7, 8); (`UTF16, 4, 5) ]
