open Common.Source
open Lang
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
