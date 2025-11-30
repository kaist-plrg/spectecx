open Common.Source

type t =
  | ParseError of region * string
  | ElabError of Pass.Elaborate.Error.elaboration_error list
  | RoundtripError of region * string
  | IlInterpError of region * string
  | SlInterpError of region * string
  | P4ParseError of region * string

val string_of_error : t -> string
