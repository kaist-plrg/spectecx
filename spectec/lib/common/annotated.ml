type ('a, 'note) annotated = { it : 'a; note : 'note; at : Location.t }
type 'a located = ('a, unit) annotated

let ( $ ) it at = { it; at; note = () }
let ( $$ ) it (at, note) = { it; at; note }
let it { it; _ } = it
let at { at; _ } = at
