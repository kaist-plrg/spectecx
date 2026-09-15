open Lang.Il

let print_label fmt label =
  match Value.flatten_case_v label with
  | _, [ "_LABEL" ], [ name ] ->
      Format.pp_print_string fmt (Value.get_text name)
  | _ -> invalid_arg "expected an ITPL field label"

let print_fields separator print_value fmt fields =
  let print_field fmt field =
    match Value.flatten_case_v field with
    | _, [ operator ], [ label; value ] when operator = separator ->
        Format.fprintf fmt "%a %s %a" print_label label separator print_value
          value
    | _ -> invalid_arg "expected an ITPL record field"
  in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
    print_field fmt (Value.get_list fields)

let rec print_type fmt typ =
  match Value.flatten_case_v typ with
  | _, [ "TOP" ], [] -> Format.pp_print_string fmt "top"
  | _, [ "BOTTOM" ], [] -> Format.pp_print_string fmt "bottom"
  | _, [ "{"; "}" ], [ fields ] ->
      Format.fprintf fmt "{%a}" (print_fields ":" print_type) fields
  | _, [ "NUM" ], [] -> Format.pp_print_string fmt "num"
  | _, [ "_TID" ], [ a ] -> Format.pp_print_string fmt (Value.get_text a)
  | _, [ "->" ], [ param; result ] ->
      Format.fprintf fmt "(%a -> %a)" print_type param print_type result
  | _, [ "FORALL"; "." ], [ a; body ] ->
      Format.fprintf fmt "(forall %a. %a)" print_type a print_type body
  | _ -> invalid_arg "expected an ITPL type"

let rec print_expr fmt expr =
  match Value.flatten_case_v expr with
  | _, [ "{"; "}" ], [ fields ] ->
      Format.fprintf fmt "{%a}" (print_fields "=" print_expr) fields
  | _, [ "." ], [ expr; label ] ->
      Format.fprintf fmt "(%a.%a)" print_expr expr print_label label
  | _, [ "_NUM" ], [ n ] ->
      Format.pp_print_string fmt
        (Bigint.to_string (Xl.Num.to_int (Value.get_num n)))
  | _, [ "_ID" ], [ x ] -> Format.pp_print_string fmt (Value.get_text x)
  | _, [ (("+" | "-" | ":=" | ";") as operator) ], [ left; right ] ->
      Format.fprintf fmt "(%a %s %a)" print_expr left operator print_expr right
  | _, [ "BOX" ], [ expr ] -> Format.fprintf fmt "(box %a)" print_expr expr
  | _, [ "!" ], [ expr ] -> Format.fprintf fmt "(!%a)" print_expr expr
  | _, [ "VAL"; "="; "IN" ], [ x; init; body ] ->
      Format.fprintf fmt "(val %a = %a in %a)" print_expr x print_expr init
        print_expr body
  | _, [ "("; ")" ], [ f; argument ] ->
      Format.fprintf fmt "%a(%a)" print_expr f print_expr argument
  | _, [ "LAMBDA"; "." ], [ x; body ] ->
      Format.fprintf fmt "(fun %a -> %a)" print_expr x print_expr body
  | _, [ "LAMBDA"; ":"; "." ], [ x; param; body ] ->
      Format.fprintf fmt "(fun (%a : %a) -> %a)" print_expr x print_type param
        print_expr body
  | _, [ "TLAMBDA"; "." ], [ a; body ] ->
      Format.fprintf fmt "(tfun %a -> %a)" print_type a print_expr body
  | _, [ "["; "]" ], [ expr; typ ] ->
      Format.fprintf fmt "(%a[%a])" print_expr expr print_type typ
  | ( _,
      [ "TYPE"; "="; "@"; "+"; "@"; "IN" ],
      [ a; left; left_type; right; right_type; body ] ) ->
      Format.fprintf fmt "(type %a = %a@%a + %a@%a in %a)" print_type a
        print_expr left print_type left_type print_expr right print_type
        right_type print_expr body
  | ( _,
      [ "MATCH"; "("; ")"; "->"; ","; "("; ")"; "->" ],
      [ subject; left; x; left_body; right; y; right_body ] ) ->
      Format.fprintf fmt "(%a match { %a(%a) -> %a; %a(%a) -> %a })" print_expr
        subject print_expr left print_expr x print_expr left_body print_expr
        right print_expr y print_expr right_body
  | _, [ "VCC"; "IN" ], [ x; body ] ->
      Format.fprintf fmt "(vcc %a in %a)" print_expr x print_expr body
  | _, [], [ func; argument ] ->
      Format.fprintf fmt "(%a (%a))" print_expr func print_expr argument
  | _, [ "IF0" ], [ condition; consequent; alternative ] ->
      Format.fprintf fmt "(if0 %a then %a else %a)" print_expr condition
        print_expr consequent print_expr alternative
  | _, [ "DEF"; "("; ")"; "="; "IN" ], [ f; x; func_body; body ] ->
      Format.fprintf fmt "(def %a(%a) = %a in %a)" print_expr f print_expr x
        print_expr func_body print_expr body
  | ( _,
      [ "DEF"; "("; ":"; ")"; ":"; "="; "IN" ],
      [ f; x; param; result; func_body; body ] ) ->
      Format.fprintf fmt "(def %a(%a : %a) : %a = %a in %a)" print_expr f
        print_expr x print_type param print_type result print_expr func_body
        print_expr body
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
