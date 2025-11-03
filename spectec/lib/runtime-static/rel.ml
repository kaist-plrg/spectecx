(* Input hints for rules *)

module Hint = struct
  type t = int list

  let to_string t =
    Format.asprintf "hint(input %s)"
      (String.concat " " (List.map (fun idx -> "%" ^ string_of_int idx) t))

  let split_exps (hint : t) (exps : Il.exp list) :
      (int * Il.exp) list * (int * Il.exp) list =
    exps
    |> List.mapi (fun idx exp -> (idx, exp))
    |> List.partition (fun (idx, _) -> List.mem idx hint)

  let split_exps_without_idx (hint : t) (exps : Il.exp list) :
      Il.exp list * Il.exp list =
    exps
    |> List.mapi (fun idx exp -> (idx, exp))
    |> List.partition (fun (idx, _) -> List.mem idx hint)
    |> fun (exps_input, exps_output) ->
    (List.map snd exps_input, List.map snd exps_output)

  let combine_exps (exps_input : (int * Il.exp) list)
      (exps_output : (int * Il.exp) list) : Il.exp list =
    exps_input @ exps_output
    |> List.sort (fun (idx_i, _) (idx_o, _) -> compare idx_i idx_o)
    |> List.map snd

  let is_conditional (hint : t) (exps : Il.exp list) : bool =
    let _, exps_output = split_exps hint exps in
    exps_output = []
end

(* Relation *)

type t = El.Ast.nottyp * Hint.t * Il.rule list

let to_string (nottyp, inputs, rules) =
  El.Print.string_of_nottyp nottyp
  ^ " " ^ Hint.to_string inputs ^ " =\n"
  ^ String.concat "\n   " (List.map Il.Print.string_of_rule rules)
