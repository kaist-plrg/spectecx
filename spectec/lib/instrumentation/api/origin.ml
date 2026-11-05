type step = Struct of Lang.Xl.Atom.t | List of int
type t = { label : string; path : step list }

let make ~label = { label; path = [] }
let extend origin ~step = { origin with path = origin.path @ [ step ] }
