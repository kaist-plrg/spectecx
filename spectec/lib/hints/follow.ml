let parse (hintexp : El.exp) : Xl.Atom.t option =
  match hintexp.it with
  | El.AtomE { it = Xl.Atom.Tag _; _ } -> None
  | El.AtomE atom -> Some atom.it
  | _ -> None

let find (hints : Il.hint list) : Xl.Atom.t option =
  List.find_map
    (fun (hint : Il.hint) ->
      match Registry.lookup hint.hintid.it with
      | Some { kind = Registry.Follow; _ } -> parse hint.hintexp
      | _ -> None)
    hints
