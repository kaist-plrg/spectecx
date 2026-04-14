open Common.Source

(* Mixop is a generalized case constructor *)

type atom = Atom.t phrase

type t =
  | Arg
  | Atom of atom
  | Brack of atom * t * atom
  | Infix of t * atom * t
  | Seq of t list

(* Normalization: flatten to list form. *)
let rec flatten_to_list (mixop : t) : t list =
  match mixop with
  | Arg | Atom _ -> [ mixop ]
  | Brack (al, inner, ar) -> (Atom al :: flatten_to_list inner) @ [ Atom ar ]
  | Infix (ml, atom, mr) ->
      flatten_to_list ml @ [ Atom atom ] @ flatten_to_list mr
  | Seq parts -> List.concat_map flatten_to_list parts

let normalize (mixop : t) : t list = flatten_to_list mixop

(* Comparison: compares on the normalized form. *)
let compare_atom (atom_a : atom) (atom_b : atom) =
  Atom.compare atom_a.it atom_b.it

let compare_primitive (a : t) (b : t) =
  match (a, b) with
  | Arg, Arg -> 0
  | Arg, _ -> -1
  | _, Arg -> 1
  | Atom atom_a, Atom atom_b -> compare_atom atom_a atom_b
  | _ -> assert false

let compare (mixop_a : t) (mixop_b : t) =
  if mixop_a == mixop_b then 0
  else List.compare compare_primitive (normalize mixop_a) (normalize mixop_b)

let eq (mixop_a : t) (mixop_b : t) = compare mixop_a mixop_b = 0

(* Arity *)
let rec arity = function
  | Arg -> 1
  | Atom _ -> 0
  | Brack (_, mixop, _) -> arity mixop
  | Infix (mixop_l, _, mixop_r) -> arity mixop_l + arity mixop_r
  | Seq mixops -> List.fold_left (fun acc mixop -> acc + arity mixop) 0 mixops

(* Extract atoms *)
let rec atoms = function
  | Arg -> []
  | Atom atom -> [ atom ]
  | Brack (atom_l, mixop, atom_r) -> (atom_l :: atoms mixop) @ [ atom_r ]
  | Infix (mixop_l, atom, mixop_r) -> atoms mixop_l @ [ atom ] @ atoms mixop_r
  | Seq mixops -> List.concat_map atoms mixops

(* --- Constructors --- *)

let arg : t = Arg
let mk_atom (s : string) : t = Atom (Atom.Atom s $ no_region)
let silent_atom (s : string) : t = Atom (Atom.SilentAtom s $ no_region)

let brack (l : string) (inner : t) (r : string) : t =
  Brack (Atom.Atom l $ no_region, inner, Atom.Atom r $ no_region)

let seq (ts : t list) : t = Seq ts

(* Assembler: interleave rendered atoms and argument strings *)

let assemble ~(string_of_atom : atom -> string) (mixop : t) (args : string list)
    : string =
  let rec go mixop args =
    match mixop with
    | Arg -> (
        match args with
        | [] -> failwith "Mixop.assemble: not enough arguments"
        | arg :: args -> (arg, args))
    | Atom atom -> (string_of_atom atom, args)
    | Brack (atom_l, mixop, atom_r) ->
        let s, args = go mixop args in
        let s =
          [ string_of_atom atom_l; s; string_of_atom atom_r ]
          |> List.filter (fun s -> s <> "")
          |> String.concat " "
        in
        (s, args)
    | Infix (mixop_l, atom, mixop_r) ->
        let s_l, args = go mixop_l args in
        let s_r, args = go mixop_r args in
        let s =
          [ s_l; string_of_atom atom; s_r ]
          |> List.filter (fun s -> s <> "")
          |> String.concat " "
        in
        (s, args)
    | Seq mixops ->
        let ss, args =
          List.fold_left
            (fun (ss, args) mixop ->
              let s, args = go mixop args in
              (ss @ [ s ], args))
            ([], args) mixops
        in
        let s = ss |> List.filter (fun s -> s <> "") |> String.concat " " in
        (s, args)
  in
  let s, args = go mixop args in
  match args with [] -> s | _ -> failwith "Mixop.assemble: too many arguments"

(* Stringifier *)

let string_of_mixop (mixop : t) =
  let rec to_string = function
    | Arg -> "%"
    | Atom atom -> Atom.string_of_atom atom.it
    | Brack (atom_l, mixop, atom_r) ->
        Atom.string_of_atom atom_l.it
        ^ to_string mixop
        ^ Atom.string_of_atom atom_r.it
    | Infix (mixop_l, atom, mixop_r) ->
        to_string mixop_l ^ Atom.string_of_atom atom.it ^ to_string mixop_r
    | Seq mixops -> String.concat " " (List.map to_string mixops)
  in
  "`" ^ to_string mixop ^ "`"
