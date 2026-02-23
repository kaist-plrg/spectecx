open Lang.Il

(* Function *)

type t = tparam list * clause list

let to_string (tparams, clauses) =
  "def"
  ^ Print.string_of_tparams tparams
  ^ "\n"
  ^ String.concat "\n"
      (List.mapi (fun idx clause -> Print.string_of_clause idx clause) clauses)
