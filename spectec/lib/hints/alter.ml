open Common.Source
open Xl

type hole = [ `Next | `Num of int ]

type t =
  | TextH of string
  | AtomH of Atom.t phrase
  | SeqH of t list
  | BrackH of Atom.t phrase * t * Atom.t phrase
  | HoleH of hole
  | FuseH of t * t
  | OtherH of El.exp

let rec to_string (h : t) : string =
  match h with
  | TextH text -> "\"" ^ String.escaped text ^ "\""
  | AtomH atom -> Atom.string_of_atom atom.it
  | SeqH parts -> String.concat " " (List.map to_string parts)
  | BrackH (atom_l, inner, atom_r) ->
      Atom.string_of_atom atom_l.it
      ^ to_string inner
      ^ Atom.string_of_atom atom_r.it
  | HoleH (`Num n) -> "%" ^ string_of_int n
  | HoleH `Next -> "%"
  | FuseH (l, r) -> to_string l ^ "#" ^ to_string r
  | OtherH exp -> El.Print.string_of_exp exp

let rec parse (exp : El.exp) : t =
  match exp.it with
  | El.TextE text -> TextH text
  | El.AtomE atom -> AtomH atom
  | El.SeqE exps -> SeqH (List.map parse exps)
  | El.BrackE (atom_l, inner, atom_r) -> BrackH (atom_l, parse inner, atom_r)
  | El.HoleE (`Num n) -> HoleH (`Num n)
  | El.HoleE `Next -> HoleH `Next
  | El.FuseE (l, r) -> FuseH (parse l, parse r)
  | _ -> OtherH exp

let rec collect (hint : t) : int list = collect' [] hint

and collect' (idxs : int list) (hint : t) : int list =
  match hint with
  | SeqH hints -> List.fold_left collect' idxs hints
  | BrackH (_, hint, _) -> collect' idxs hint
  | HoleH (`Num n) -> n :: idxs
  | FuseH (hint_l, hint_r) ->
      let idxs = collect' idxs hint_l in
      collect' idxs hint_r
  | _ -> idxs

let rec realign (hint : t) (inputs : Input.t) : t =
  let outputs = collect hint in
  let all = inputs @ outputs |> List.sort_uniq compare in
  let mapping =
    List.fold_left
      (fun acc idx ->
        if List.mem idx outputs then
          let idx_realigned = List.length acc in
          acc @ [ (idx, idx_realigned) ]
        else acc)
      [] all
  in
  realign' mapping hint

and realign' (mapping : (int * int) list) (hint : t) : t =
  match hint with
  | SeqH hints -> SeqH (List.map (realign' mapping) hints)
  | BrackH (atom_l, hint, atom_r) ->
      BrackH (atom_l, realign' mapping hint, atom_r)
  | HoleH (`Num n) -> HoleH (`Num (List.assoc n mapping))
  | FuseH (hint_l, hint_r) ->
      FuseH (realign' mapping hint_l, realign' mapping hint_r)
  | _ -> hint
