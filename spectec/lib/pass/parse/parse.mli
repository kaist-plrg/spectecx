type error
type 'a result = ('a, error) Stdlib.result

val parse_file : string -> Lang.El.spec result
val parse_files : string list -> Lang.El.spec result
val parse_plaintyp : string -> Lang.El.plaintyp result
val parse_prem : string -> Lang.El.prem result
val error_to_string : error -> string
val error_to_diagnostic : error -> Diag.t
