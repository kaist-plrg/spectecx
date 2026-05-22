type error

val error_to_string : error -> string
val error_to_diagnostics : error -> Diag.Bag.t
val parse_files : string list -> (Lang.El.spec, error) result

type il = Elaborate.il = { lang : Lang.Il.spec; qc : Qc_il.spec }

val elaborate : Lang.El.spec -> (il, error) result
val structure : Lang.Il.spec -> Lang.Sl.spec
