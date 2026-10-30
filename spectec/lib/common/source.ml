(* Phrases *)

type ('a, 'b) info = { it : 'a; note : 'b; at : Location.t }
type ('a, 'b) note_phrase = ('a, 'b) info
type 'a phrase = ('a, unit) info

let ( $ ) it at = { it; at; note = () }
let ( $$ ) it (at, note) = { it; at; note }
let it { it; _ } = it
let at { at; _ } = at
