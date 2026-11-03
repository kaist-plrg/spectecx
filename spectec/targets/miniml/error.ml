module Location = Common.Location

exception MinimlParseError of Location.t * string

let error (at : Location.t) (msg : string) = raise (MinimlParseError (at, msg))
