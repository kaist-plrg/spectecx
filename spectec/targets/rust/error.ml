open Common.Source

exception RustParseError of region * string

let error (at : region) (msg : string) = raise (RustParseError (at, msg))
