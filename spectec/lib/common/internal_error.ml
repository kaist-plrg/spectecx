exception Disallowed of { loc : Location.t; msg : string }

let disallowed loc msg = raise (Disallowed { loc; msg })
