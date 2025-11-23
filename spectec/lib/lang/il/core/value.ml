open Types
open Common.Source
open Effects

type t = value

let rec compare (value_l : t) (value_r : t) =
  let tag (value : t) =
    match value.it with
    | BoolV _ -> 0
    | NumV _ -> 1
    | TextV _ -> 2
    | StructV _ -> 3
    | CaseV _ -> 4
    | TupleV _ -> 5
    | OptV _ -> 6
    | ListV _ -> 7
    | FuncV _ -> 8
  in
  match (value_l.it, value_r.it) with
  | BoolV b_l, BoolV b_r -> Stdlib.compare b_l b_r
  | NumV n_l, NumV n_r -> Xl.Num.compare n_l n_r
  | TextV s_l, TextV s_r -> String.compare s_l s_r
  | StructV fields_l, StructV fields_r ->
      let atoms_l, values_l = List.split fields_l in
      let atoms_r, values_r = List.split fields_r in
      let cmp_atoms = List.compare Xl.Atom.compare atoms_l atoms_r in
      if cmp_atoms <> 0 then cmp_atoms else compares values_l values_r
  | CaseV (mixop_l, values_l), CaseV (mixop_r, values_r) ->
      let cmp_mixop = Xl.Mixop.compare mixop_l mixop_r in
      if cmp_mixop <> 0 then cmp_mixop else compares values_l values_r
  | TupleV values_l, TupleV values_r -> compares values_l values_r
  | OptV value_opt_l, OptV value_opt_r -> (
      match (value_opt_l, value_opt_r) with
      | Some value_l, Some value_r -> compare value_l value_r
      | Some _, None -> 1
      | None, Some _ -> -1
      | None, None -> 0)
  | ListV values_l, ListV values_r -> compares values_l values_r
  | _ -> Int.compare (tag value_l) (tag value_r)

and compares (values_l : t list) (values_r : t list) : int =
  match (values_l, values_r) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | value_l :: values_l, value_r :: values_r ->
      let cmp = compare value_l value_r in
      if cmp <> 0 then cmp else compares values_l values_r

let eq (value_l : t) (value_r : t) : bool = compare value_l value_r = 0

let with_fresh_vid (typ : typ') : vnote =
  let vid = Effect.perform FreshVid () in
  { vid; typ }

let make_val (typ : typ') (v : value') : t =
  let value = v $$$ with_fresh_vid typ in
  Effect.perform (ValueCreated value);
  value

module Make = struct
  let value (t' : typ') (v : value') : t = make_val t' v
  let bool (t' : typ') (b : bool) : t = make_val t' (BoolV b)
  let num (t' : typ') (n : num) : t = make_val t' (NumV n)
  let nat (t' : typ') (n : Bigint.t) : t = make_val t' (NumV (`Nat n))
  let int (t' : typ') (n : Bigint.t) : t = make_val t' (NumV (`Int n))
  let text (t' : typ') (s : string) : t = make_val t' (TextV s)
  let tuple (t' : typ') (vs : t list) : t = make_val t' (TupleV vs)

  let record (t' : typ') (fs : valuefield list) : value =
    make_val t' (StructV fs)

  let opt (t' : typ') (v : t option) : t = make_val t' (OptV v)
  let list (t' : typ') (vs : t list) : t = make_val t' (ListV vs)

  let case (t' : typ') (cases : mixop * value list) : t =
    make_val t' (CaseV cases)
end

let get_bool (value : t) =
  match value.it with BoolV b -> b | _ -> failwith "get_bool"

let get_num (value : t) =
  match value.it with NumV n -> n | _ -> failwith "get_num"

let get_text (value : t) =
  match value.it with TextV s -> s | _ -> failwith "get_text"

let get_list (value : t) =
  match value.it with ListV values -> values | _ -> failwith "unseq"

let get_opt (value : t) =
  match value.it with OptV value -> value | _ -> failwith "get_opt"

let get_struct (value : t) =
  match value.it with StructV fields -> fields | _ -> failwith "get_struct"

let bool (b : bool) : t = Make.bool Typ.bool b
let nat (i : Bigint.t) : t = Make.nat Typ.nat i
let int (i : Bigint.t) : t = Make.int Typ.int i
let text (s : string) : t = Make.text Typ.text s
let func (id : id) : t = FuncV id |> make_val Typ.func

let tuple (vs : t list) : t =
  let typs = List.map (fun v -> v.note.typ $ no_region) vs in
  TupleV vs |> make_val (Typ.tuple typs)

let opt (typ : typ) (v : t option) : t = OptV v |> make_val (Typ.opt typ)
let list (typ : typ) (vs : t list) : t = ListV vs |> make_val (Typ.list typ)
