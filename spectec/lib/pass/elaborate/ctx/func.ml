open Lang

type t =
  | Builtin of Il.tparam list * Il.param list * Il.typ
  | Defined of Il.tparam list * Il.param list * Il.typ * Il.clause list

let to_string = function
  | Builtin (tparams, params, typ) ->
      "builtin dec "
      ^ Il.Print.string_of_tparams tparams
      ^ Il.Print.string_of_params params
      ^ " : " ^ Il.Print.string_of_typ typ
  | Defined (tparams, params, typ, clauses) ->
      "dec "
      ^ Il.Print.string_of_tparams tparams
      ^ Il.Print.string_of_params params
      ^ " : " ^ Il.Print.string_of_typ typ ^ " =\n"
      ^ String.concat "\n"
          (List.mapi
             (fun idx clause -> Il.Print.string_of_clause idx clause)
             clauses)
