(** Backend-specific IL declarations consumed by the quickcheck backend. *)

type def =
  | BuiltinGeneratorD of Il.id * Il.typ * Il.hint list
  | PropertyD of Il.id * Il.prem list * Il.prem * Il.hint list

type spec = def list

val empty : spec
