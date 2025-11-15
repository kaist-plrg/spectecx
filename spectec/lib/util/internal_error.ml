open Source

exception Disallowed of { loc : region; msg : string }

let disallowed loc msg = raise (Disallowed { loc; msg })
