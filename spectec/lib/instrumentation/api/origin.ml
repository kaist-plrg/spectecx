type step =
  | Field of Lang.Xl.Atom.t
  | Index of int
  | Opt
  | Tuple of int
  | Case of int

type t = { source : string; path : step list }

let make ~source = { source; path = [] }
let extend origin ~step = { origin with path = origin.path @ [ step ] }
let compare = Stdlib.compare

let string_of_step = function
  | Field atom -> "." ^ Lang.Xl.Atom.to_string atom
  | Index index -> Printf.sprintf "[%d]" index
  | Opt -> "?"
  | Tuple index -> Printf.sprintf ".%d" index
  | Case index -> Printf.sprintf "#%d" index

let to_string { source; path } =
  source ^ String.concat "" (List.map string_of_step path)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)
