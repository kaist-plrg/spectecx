open Lang.Il

let rec print_expr fmt expr =
  match Value.flatten_case_v expr with
  | _, [ "_NUM" ], [ n ] ->
      Format.pp_print_string fmt
        (Bigint.to_string (Xl.Num.to_int (Value.get_num n)))
  | _, [ "_ID" ], [ x ] -> Format.pp_print_string fmt (Value.get_text x)
  | _, [ (("+" | "-") as operator) ], [ left; right ] ->
      Format.fprintf fmt "(%a %s %a)" print_expr left operator print_expr right
  | _, [ "VAL"; "="; "IN" ], [ x; init; body ] ->
      Format.fprintf fmt "(val %a = %a in %a)" print_expr x print_expr init
        print_expr body
  | _ -> invalid_arg "expected an ITPL expression"

let unparse ~spec:_ = function
  | [ expr ] -> Format.asprintf "%a" print_expr expr
  | _ -> invalid_arg "expected one ITPL expression"
