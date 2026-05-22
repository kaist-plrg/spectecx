(** Final report describing keys that were registered but never referenced by
    any anchor. *)

(** One entry per anchor name. [total] is the number of registered keys;
    [unused] is the keys that were never looked up by any splice. *)
type per_anchor = { name : string; total : int; unused : string list }

type t = per_anchor list

let of_stores (stores : (string * Store.t) list) : t =
  stores
  |> List.map (fun (name, sto) ->
         { name; total = Store.cardinal sto; unused = Store.unused sto })
  |> List.sort (fun a b -> String.compare a.name b.name)

let to_string (rep : t) : string =
  let buf = Buffer.create 1024 in
  List.iter
    (fun { name; total; unused } ->
      let count = List.length unused in
      let percent =
        if total = 0 then 0.0
        else float_of_int count /. float_of_int total *. 100.0
      in
      Buffer.add_string buf
        (Printf.sprintf "# %s: %d/%d unused (%.2f%%)\n" name count total percent);
      List.iter (fun key -> Buffer.add_string buf (key ^ "\n")) unused;
      Buffer.add_char buf '\n')
    rep;
  Buffer.contents buf
