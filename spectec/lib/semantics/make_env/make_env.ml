open Common.Source
open Common.Domain

(* ========== Map-based container ========== *)

module IdMap = struct
  include Map.Make (Id)

  type 'v to_string_v = 'v -> string

  let keys m = List.map fst (bindings m)
  let dom m = m |> keys |> IdSet.of_list
  let values m = List.map snd (bindings m)

  let to_string ?(with_braces = true) ?(bind = " : ")
      (to_string_v : 'v to_string_v) m =
    let to_string_binding (k, v) = Id.to_string k ^ bind ^ to_string_v v in
    let bindings = bindings m in
    let smap = String.concat ", " (List.map to_string_binding bindings) in
    if with_braces then "{ " ^ smap ^ " }" else smap

  let extend env_a env_b =
    List.fold_left (fun env (k, v) -> add k v env) env_a (bindings env_b)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

module TIdMap = IdMap
module RIdMap = IdMap
module FIdMap = IdMap

(* ========== Env functors (add typed to_string) ========== *)

module type VALUE = sig
  type t

  val to_string : t -> string
end

module MakeIdMap (V : VALUE) = struct
  include IdMap

  type t = V.t IdMap.t

  let to_string ?(with_braces = true) ?(bind = " : ") env =
    IdMap.to_string ~with_braces ~bind V.to_string env

  let find id env =
    match find_opt id env with Some value -> value | None -> assert false
end

module MakeTIdMap = MakeIdMap
module MakeRIdMap = MakeIdMap
module MakeFIdMap = MakeIdMap

(* ========== Frozen env functors (hashtable-based) ========== *)

module IdHash = struct
  type t = Id.t

  let equal a b = String.equal a.it b.it
  let hash a = Hashtbl.hash a.it
end

module IdHashtbl = Hashtbl.Make (IdHash)

(* The loader is mutable during loading, then we freeze it.
   We wrap the frozen hashtable to prevent mutation. *)
module MakeFrozenIdTbl (V : VALUE) = struct
  type loader = V.t IdHashtbl.t
  type t = Frozen of V.t IdHashtbl.t

  let create () = IdHashtbl.create 16
  let add l k v = IdHashtbl.replace l k v
  let freeze l = Frozen l
  let find_opt k (Frozen t) = IdHashtbl.find_opt t k

  let find k (Frozen t) =
    match IdHashtbl.find_opt t k with Some v -> v | None -> assert false
end

module MakeFrozenTIdTbl = MakeFrozenIdTbl
module MakeFrozenRIdTbl = MakeFrozenIdTbl
module MakeFrozenFIdTbl = MakeFrozenIdTbl
