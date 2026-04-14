open Lang

[@@@ocamlformat "disable"]

type t =
  | Param
  | Defining of Il.tparam list
  | Defined of Il.tparam list * Il.deftyp
[@@@ocamlformat "enable"]

let to_string = function
  | Param -> "Param"
  | Defining tparams -> "Defining" ^ Il.Print.string_of_tparams tparams
  | Defined (tparams, deftyp) ->
      "Defined"
      ^ Il.Print.string_of_tparams tparams
      ^ " = "
      ^ Il.Print.string_of_deftyp deftyp

let get_tparams = function
  | Param -> []
  | Defining tparams -> tparams
  | Defined (tparams, _) -> tparams
