type error =
  | ParseError of string
  | ElabError of string
  | NoManualGenerator of string

type 'a result = ('a, error) Stdlib.result

val quickcheck_file :
  Lang.Il.spec -> string -> unit result

val error_to_string : error -> string
val error_to_diagnostic : error -> Diagnostic.t
