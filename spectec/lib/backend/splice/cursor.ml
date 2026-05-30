(** Mutable string cursor used by the splice driver.

    The cursor walks the input file from beginning to end exactly once. Its
    mutation lives entirely inside one call to {!Driver.splice_file}: the cursor
    is allocated at the top of that function, advanced forward, and discarded at
    the end. No caller of [Driver] observes the mutation. *)

open Common.Source

type t = { file : string; s : string; mutable i : int }

let make ~file s = { file; s; i = 0 }
let eos cur = cur.i >= String.length cur.s
let peek cur = cur.s.[cur.i]
let adv cur = cur.i <- cur.i + 1

let starts_with cur prefix =
  let n = String.length prefix in
  let left = String.length cur.s - cur.i in
  if left < n then false
  else
    let rec loop k =
      k = n || (cur.s.[cur.i + k] = prefix.[k] && loop (k + 1))
    in
    loop 0

let consume cur prefix =
  if starts_with cur prefix then (
    cur.i <- cur.i + String.length prefix;
    true)
  else false

let pos cur =
  let line = ref 1 in
  let col = ref 1 in
  for j = 0 to cur.i - 1 do
    if cur.s.[j] = '\n' then (
      incr line;
      col := 1)
    else incr col
  done;
  { file = cur.file; line = !line; column = !col }

let region cur =
  let p = pos cur in
  { left = p; right = p }
