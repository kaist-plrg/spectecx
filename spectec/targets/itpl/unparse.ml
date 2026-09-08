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
  | _, [ "("; ")" ], [ f; argument ] ->
      Format.fprintf fmt "%a(%a)" print_expr f print_expr argument
  | _, [ "LAMBDA"; "." ], [ x; body ] ->
      Format.fprintf fmt "(fun %a -> %a)" print_expr x print_expr body
  | _, [], [ func; argument ] ->
      Format.fprintf fmt "(%a (%a))" print_expr func print_expr argument
  | _, [ "IF0" ], [ condition; consequent; alternative ] ->
      Format.fprintf fmt "(if0 %a then %a else %a)" print_expr condition
        print_expr consequent print_expr alternative
  | _, [ "DEF"; "("; ")"; "="; "IN" ], [ f; x; func_body; body ] ->
      Format.fprintf fmt "(def %a(%a) = %a in %a)" print_expr f print_expr x
        print_expr func_body print_expr body
  | _ -> invalid_arg "expected an ITPL expression"

let print_fundef fmt definition =
  match Value.flatten_case_v definition with
  | _, [ "DEF"; "("; ")"; "=" ], [ f; x; body ] ->
      Format.fprintf fmt "def %a(%a) = %a; " print_expr f print_expr x
        print_expr body
  | _ -> invalid_arg "expected an ITPL function definition"

let print_fbinding fmt binding =
  match Value.flatten_case_v binding with
  | _, [ "->" ], [ _; definition ] -> print_fundef fmt definition
  | _ -> invalid_arg "expected an ITPL function binding"

let print_input fmt input =
  match Value.flatten_case_v input with
  | _, [ "_PROGRAM" ], [ bindings; expr ] ->
      List.iter (print_fbinding fmt) (Value.get_list bindings);
      print_expr fmt expr
  | _ -> print_expr fmt input

let unparse ~spec:_ = function
  | [ input ] -> Format.asprintf "%a" print_input input
  | _ -> invalid_arg "expected one ITPL input"
