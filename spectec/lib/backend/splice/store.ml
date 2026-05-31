(** Per-anchor key/value store with used-set tracking.

    Allocated locally inside {!Driver.run}. The [used] mutation cannot escape
    the lifetime of that call: when [run] returns, the stores become
    unreachable. *)

type t = {
  entries : (string, string) Hashtbl.t;
  used : (string, unit) Hashtbl.t;
}

let create (pairs : (string * string) list) : t =
  let entries = Hashtbl.create (List.length pairs) in
  List.iter (fun (k, v) -> Hashtbl.replace entries k v) pairs;
  { entries; used = Hashtbl.create 16 }

let cardinal sto = Hashtbl.length sto.entries
let find_opt sto key = Hashtbl.find_opt sto.entries key
let mark_used sto key = Hashtbl.replace sto.used key ()
let is_used sto key = Hashtbl.mem sto.used key

let unused sto : string list =
  Hashtbl.fold
    (fun k _ acc -> if Hashtbl.mem sto.used k then acc else k :: acc)
    sto.entries []
  |> List.sort String.compare
