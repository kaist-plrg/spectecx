(** Field-name hints for variant case constructors.

    A [hint(fields name_1 name_2 ...)] on a [typcase] names each positional
    argument of the constructor. The shorthand pass uses these names to lift a
    [LetI] whose LHS is a [CaseE] pattern into a [DestructI] that mentions each
    field by name. *)

type t = string list

let to_string (hint : t) : string =
  Format.asprintf "hint(fields %s)"
    (hint
    |> List.map (fun s -> "\"" ^ String.escaped s ^ "\"")
    |> String.concat " ")

let parse (hintexp : El.exp) : t option =
  match hintexp.it with
  | El.TextE text -> Some [ text ]
  | El.SeqE exps ->
      List.fold_left
        (fun acc (exp : El.exp) ->
          match acc with
          | None -> None
          | Some names -> (
              match exp.it with
              | El.TextE text -> Some (names @ [ text ])
              | _ -> None))
        (Some []) exps
  | _ -> None
