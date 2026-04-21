open Common.Source

(* IL mixop: a sequence of atoms and argument placeholders. *)

type atom = Xl.Atom.t phrase
type mixeme = Arg | Atom of atom
type t = mixeme list

(* Comparison *)

let compare_atom (atom_a : atom) (atom_b : atom) =
  Xl.Atom.compare atom_a.it atom_b.it

let compare_mixeme (a : mixeme) (b : mixeme) =
  match (a, b) with
  | Arg, Arg -> 0
  | Arg, Atom _ -> -1
  | Atom _, Arg -> 1
  | Atom atom_a, Atom atom_b -> compare_atom atom_a atom_b

let compare (mixop_a : t) (mixop_b : t) =
  List.compare compare_mixeme mixop_a mixop_b

let eq (mixop_a : t) (mixop_b : t) = compare mixop_a mixop_b = 0

(* Arity *)

let arity (mixop : t) : int =
  List.fold_left (fun n p -> match p with Arg -> n + 1 | Atom _ -> n) 0 mixop

(* Atoms in sequence *)

let atoms (mixop : t) : atom list =
  List.filter_map (function Atom a -> Some a | Arg -> None) mixop

(* Assembler: interleave rendered atoms and argument strings, space-joined *)

let assemble ~(string_of_atom : atom -> string) (mixop : t) (args : string list)
    : string =
  let rec assemble' mixop args mixemes_rev =
    match (mixop, args) with
    | [], [] -> List.rev mixemes_rev
    | [], _ :: _ -> failwith "Mixop.assemble: too many arguments"
    | Arg :: _, [] -> failwith "Mixop.assemble: not enough arguments"
    | Arg :: rest, arg :: args_rest ->
        assemble' rest args_rest (arg :: mixemes_rev)
    | Atom atom :: rest, args ->
        assemble' rest args (string_of_atom atom :: mixemes_rev)
  in
  assemble' mixop args [] |> List.filter (fun s -> s <> "") |> String.concat " "

(* Stringifier

   Workaround to match the output of tree-mixops.
   Omit the space after an open bracket, and before a close bracket. *)

let is_open_bracket : Xl.Atom.t -> bool = function
  | LParen | LBrack | LBrace | LAngle -> true
  | _ -> false

let is_close_bracket : Xl.Atom.t -> bool = function
  | RParen | RBrack | RBrace | RAngle -> true
  | _ -> false

let opens_bracket = function Atom a -> is_open_bracket a.it | Arg -> false
let closes_bracket = function Atom a -> is_close_bracket a.it | Arg -> false

let string_of_mixeme = function
  | Arg -> "%"
  | Atom atom -> Xl.Atom.string_of_atom atom.it

let string_of_mixop (mixop : t) : string =
  let rec string_of_mixemes = function
    | [] -> ""
    | [ mixeme ] -> string_of_mixeme mixeme
    | left :: (right :: _ as rest) ->
        let sep =
          if opens_bracket left || closes_bracket right then "" else " "
        in
        string_of_mixeme left ^ sep ^ string_of_mixemes rest
  in
  "`" ^ string_of_mixemes mixop ^ "`"
