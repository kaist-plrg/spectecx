open Lang.Il

(* Type definition *)

type t = tparam list * deftyp

let to_string (tparams, deftyp) =
  Print.string_of_tparams tparams ^ " " ^ Print.string_of_deftyp deftyp
