(* Input hints for rules *)

type t = int list

let split (hint : t) (items : 'a list) : 'a list * 'a list =
  items
  |> List.mapi (fun idx item -> (idx, item))
  |> List.partition (fun (idx, _) -> List.mem idx hint)
  |> fun (item_input, item_output) ->
  (List.map snd item_input, List.map snd item_output)

let combine (hint : t) (items_input : 'a list) (items_output : 'a list) :
    'a list =
  let len = List.length items_input + List.length items_output in
  let idxs_input, idxs_output =
    List.init len Fun.id |> List.partition (fun idx -> List.mem idx hint)
  in
  let items_input_indexed = List.combine idxs_input items_input in
  let items_output_indexed = List.combine idxs_output items_output in
  items_input_indexed @ items_output_indexed
  |> List.sort (fun (idx_a, _) (idx_b, _) -> Int.compare idx_a idx_b)
  |> List.map snd

(* Parsing *)

let parse (hintexp : El.exp) : t option =
  let open Common.Source in
  let collect_hole (exp : El.exp) =
    match exp.it with El.HoleE (`Num input) -> Some input | _ -> None
  in
  match hintexp.it with
  | El.SeqE exps ->
      List.fold_left
        (fun acc exp ->
          match acc with
          | None -> None
          | Some inputs -> (
              match collect_hole exp with
              | Some input -> Some (inputs @ [ input ])
              | None -> None))
        (Some []) exps
  | El.HoleE (`Num input) -> Some [ input ]
  | _ -> None
