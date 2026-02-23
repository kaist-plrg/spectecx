open Lang
(* Cache entry for relation and function invocations *)

module Entry = struct
  type t = string * Il.Value.t list

  let equal (id_a, values_a) (id_b, values_b) =
    id_a = id_b
    && List.compare (fun v_a v_b -> Il.Value.compare v_a v_b) values_a values_b
       = 0

  (* Structural hash matching the structural equality in Il.Value.compare *)

  (* Infix operator for combining hash values. *)

  let ( +! ) h1 h2 = (h1 * 65599) + h2
  let hash_atom (atom : Xl.Atom.t) : int = Hashtbl.hash atom

  let hash_num (num : Xl.Num.t) : int =
    match num with `Nat n -> 0 +! Bigint.hash n | `Int i -> 1 +! Bigint.hash i

  let hash_mixop (mixop : Xl.Mixop.t) : int =
    List.fold_left
      (fun hash atoms ->
        List.fold_left
          (fun hash atom -> hash +! hash_atom atom.Common.Source.it)
          hash atoms)
      2 mixop

  let rec hash_value (v : Il.Value.t) : int =
    match v.it with
    | BoolV b -> 0 +! Hashtbl.hash b
    | NumV n -> 1 +! hash_num n
    | TextV s -> 2 +! Hashtbl.hash s
    | StructV fields ->
        List.fold_left
          (fun hash (atom, v) ->
            hash +! (hash_atom atom.Common.Source.it +! hash_value v))
          3 fields
    | CaseV (mixop, values) ->
        let base_hash = 4 +! hash_mixop mixop in
        List.fold_left (fun hash v -> hash +! hash_value v) base_hash values
    | TupleV values ->
        List.fold_left (fun hash v -> hash +! hash_value v) 5 values
    | OptV None -> 6
    | OptV (Some v) -> 7 +! hash_value v
    | ListV values ->
        List.fold_left (fun hash v -> hash +! hash_value v) 8 values
    | FuncV id -> 9 +! Hashtbl.hash id.Common.Source.it

  let hash (id, values) =
    let base_hash = Hashtbl.hash id in
    List.fold_left (fun hash v -> hash +! hash_value v) base_hash values
end

(* LFU (with LRU tiebreak) cache over Entry keys *)

module Cache = struct
  module Table = Hashtbl.Make (Entry)

  let create ~size = Table.create size
  let clear cache = Table.clear cache
  let find cache key = Table.find_opt cache key
  let add cache key value = Table.add cache key value
end

let is_cached_func = function
  | "subst_type" | "subst_typeDef" | "specialize_typeDef" | "canon"
  | "free_type" | "is_nominal_typeIR" | "bound" | "gen_constraint_type"
  | "merge_constraint" | "merge_constraint'" | "find_matchings"
  | "nestable_struct" | "nestable_struct_in_header" | "find_map" ->
      true
  | _ -> false

let is_cached_rule = function
  | "Sub_expl" | "Sub_expl_canon" | "Sub_expl_canon_neq" | "Sub_impl"
  | "Sub_impl_canon" | "Sub_impl_canon_neq" | "Type_wf" | "Type_alpha" ->
      true
  | _ -> false

let with_cache cache (id, values) compute =
  let key = (id, values) in
  match Cache.find !cache key with
  | Some v -> Ok v
  | None ->
      let result = compute () in
      (match result with Ok v -> Cache.add !cache key v | _ -> ());
      result
