type 'a typdef = {
  name : string;
  identity : 'a Type.Id.t;
  compare : 'a -> 'a -> int;
  hash : 'a -> int;
}

type typdef_as_any = Typdef : 'a typdef -> typdef_as_any
type value = Value : 'a typdef * 'a -> value

let make_typdef ~name ~compare ~hash =
  { name; identity = Type.Id.make (); compare; hash }

let typdef_name typdef = typdef.name
let name (Value (td, _)) = td.name
let of_ocaml ~typdef value = Value (typdef, value)

let to_ocaml_opt (type a) ~(typdef : a typdef) (Value (td_actual, v)) : a option
    =
  match Type.Id.provably_equal typdef.identity td_actual.identity with
  | Some Type.Equal -> Some v
  | None -> None

let has_type (type a) (Value (td_actual, _)) ~(typdef : a typdef) =
  match Type.Id.provably_equal typdef.identity td_actual.identity with
  | Some Type.Equal -> true
  | None -> false

let compare (Value (td_left, v_left)) (Value (td_right, v_right)) =
  match Type.Id.provably_equal td_left.identity td_right.identity with
  | Some Type.Equal -> td_left.compare v_left v_right
  | None ->
      let cmp = String.compare td_left.name td_right.name in
      if cmp <> 0 then cmp
      else
        Int.compare
          (Type.Id.uid td_left.identity)
          (Type.Id.uid td_right.identity)

let hash (Value (td, v)) = Hashtbl.hash (Type.Id.uid td.identity, td.hash v)
