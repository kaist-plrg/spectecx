open Common.Source

type t =
  | ParseError of region * string
  | RoundtripError of region * string
  | ElaborateError of Pass.Elaborate.elaboration_error list
  | EvalIlError of region * string
  | EvalSlError of region * string
  | TaskParseError of region * string
  | SpecMismatchError of string * string
  | DirectoryError of string

val string_of_error : t -> string
