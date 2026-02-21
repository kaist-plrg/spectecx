open Common.Source

exception Error of region * string

val parse_file : string -> Lang.El.spec
