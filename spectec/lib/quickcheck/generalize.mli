open Lang.Il

val show_env : (id' * value) list -> string

val generalize_env :
  spec ->
  (id' * value) list ->
  (string * ((id' * value) list Gen.t)) list