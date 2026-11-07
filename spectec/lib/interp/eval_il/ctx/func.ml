open Lang.Il

(* Function *)

type t =
  | Builtin of tparam list * typ list * typ
  | Defined of tparam list * typ list * clause list

let signature = function
  | Builtin (tparams, typs, _) | Defined (tparams, typs, _) -> (tparams, typs)

let to_string = function
  | Builtin _ -> "builtin function"
  | Defined (tparams, _, clauses) ->
      "def"
      ^ Print.string_of_tparams tparams
      ^ "\n"
      ^ String.concat "\n"
          (List.mapi
             (fun idx clause -> Print.string_of_clause idx clause)
             clauses)
