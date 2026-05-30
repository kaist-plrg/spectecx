(** Group [(key, value)] pairs by key, preserving the order in which each key
    was first encountered. *)

let in_order (pairs : (string * 'a) list) : (string * 'a list) list =
  let table = Hashtbl.create 16 in
  let order = ref [] in
  List.iter
    (fun (key, v) ->
      if not (Hashtbl.mem table key) then order := key :: !order;
      let prev = try Hashtbl.find table key with Not_found -> [] in
      Hashtbl.replace table key (prev @ [ v ]))
    pairs;
  List.rev_map (fun key -> (key, Hashtbl.find table key)) !order
