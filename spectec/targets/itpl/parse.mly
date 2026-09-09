%{
open Lang.Il.Value
open Lang.Il.Case
open Common.Source

let number n = [tag "NUM"; arg (int n)] |> case_v ~var:"expr"
let identifier x = [tag "ID"; arg (text x)] |> case_v ~var:"id"
let binary operator left right =
  [arg left; op operator; arg right] |> case_v ~var:"expr"
let binding x init body =
  [kw "VAL"; arg (identifier x); op "="; arg init; kw "IN"; arg body]
  |> case_v ~var:"expr"
let call f arg_expr =
  [arg (identifier f); lparen (); arg arg_expr; rparen ()]
  |> case_v ~var:"expr"
let function_binding f x body =
  let definition =
    [kw "DEF"; arg (identifier f); lparen (); arg (identifier x); rparen ();
     op "="; arg body] |> case_v ~var:"fundef"
  in
  [arg (identifier f); atom Lang.Xl.Atom.Arrow; arg definition] |> case_v ~var:"fbinding"
let program bindings expr =
  [tag "PROGRAM"; arg (list (Lang.Il.Typ.var "fbinding" [] $ no_region) bindings); arg expr]
  |> case_v ~var:"prog"
let function_expr x body =
  [kw "LAMBDA"; arg (identifier x); op "."; arg body] |> case_v ~var:"expr"
let application func argument = [arg func; arg argument] |> case_v ~var:"expr"
let let_expr x init body = application (function_expr x body) init
let rec contains_identifier name expr =
  match flatten_case_v expr with
  | _, ["_ID"], [x] -> String.equal name (get_text x)
  | _, ["_NUM"], _ -> false
  | _, _, args -> List.exists (contains_identifier name) args
let sequence_sugar first second =
  let rec fresh name =
    if contains_identifier name second then fresh (name ^ "'") else name
  in
  let_expr (fresh "__seq") first second
let new_box expr = [kw "BOX"; arg expr] |> case_v ~var:"expr"
let open_box expr = [op "!"; arg expr] |> case_v ~var:"expr"
let capture x body =
  [kw "VCC"; arg (identifier x); kw "IN"; arg body] |> case_v ~var:"expr"
let conditional condition consequent alternative =
  [kw "IF0"; arg condition; arg consequent; arg alternative]
  |> case_v ~var:"expr"
let recursive_binding f x func_body body =
  [kw "DEF"; arg (identifier f); lparen (); arg (identifier x); rparen ();
   op "="; arg func_body; kw "IN"; arg body] |> case_v ~var:"expr"
%}

%token EOF
%token <Bigint.t> NUM
%token <string> ID
%token PLUS MINUS LPAR RPAR VAL IN EQUAL DEF SEMI FUN ARROW IF0 THEN ELSE BOX BANG ASSIGN VCC

%start <Lang.Il.value> ae vae f1vae fvae fae rfae bfae mfae kfae

%%

ae:
| e=ae_expr EOF { e }

vae:
| e=vae_expr EOF { e }

f1vae:
| bindings=fbinding* e=f1vae_expr EOF { program bindings e }

fvae:
| e=fvae_expr EOF { e }

fae:
| e=fae_expr EOF { e }

rfae:
| e=rfae_expr EOF { e }

bfae:
| e=bfae_expr EOF { e }

mfae:
| e=mfae_expr EOF { e }

kfae:
| e=kfae_expr EOF { e }

ae_expr:
| e=ae_atom { e }
| left=ae_expr PLUS right=ae_atom { binary "+" left right }
| left=ae_expr MINUS right=ae_atom { binary "-" left right }

ae_atom:
| n=integer { number n }
| LPAR e=ae_expr RPAR { e }

vae_expr:
| e=vae_sum { e }
| VAL x=vae_id EQUAL init=vae_expr IN body=vae_expr { binding x init body }

vae_sum:
| e=vae_atom { e }
| left=vae_sum PLUS right=vae_atom { binary "+" left right }
| left=vae_sum MINUS right=vae_atom { binary "-" left right }

vae_atom:
| n=integer { number n }
| x=vae_id { identifier x }
| LPAR e=vae_expr RPAR { e }

vae_id:
| x=ID { x }
| DEF { "def" }
| FUN { "fun" }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

f1vae_id:
| x=ID { x }
| FUN { "fun" }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

fvae_id:
| x=ID { x }
| DEF { "def" }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

fae_id:
| x=ID { x }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

rfae_id:
| x=ID { x }
| BOX { "box" }
| VCC { "vcc" }

bfae_id:
| x=ID { x }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| VCC { "vcc" }

mfae_id:
| x=ID { x }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

fbinding:
| DEF f=f1vae_id LPAR x=f1vae_id RPAR EQUAL body=f1vae_expr SEMI
    { function_binding f x body }

f1vae_expr:
| e=f1vae_sum { e }
| VAL x=f1vae_id EQUAL init=f1vae_expr IN body=f1vae_expr { binding x init body }

f1vae_sum:
| e=f1vae_atom { e }
| left=f1vae_sum PLUS right=f1vae_atom { binary "+" left right }
| left=f1vae_sum MINUS right=f1vae_atom { binary "-" left right }

f1vae_atom:
| n=integer { number n }
| x=f1vae_id { identifier x }
| f=f1vae_id LPAR e=f1vae_expr RPAR { call f e }
| LPAR e=f1vae_expr RPAR { e }

fvae_expr:
| e=fvae_sum { e }
| VAL x=fvae_id EQUAL init=fvae_expr IN body=fvae_expr { binding x init body }
| FUN x=fvae_id ARROW body=fvae_expr { function_expr x body }

fvae_sum:
| e=fvae_app { e }
| left=fvae_sum PLUS right=fvae_app { binary "+" left right }
| left=fvae_sum MINUS right=fvae_app { binary "-" left right }

fvae_app:
| e=fvae_atom { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=fvae_app argument=fvae_atom { application func argument }

fvae_atom:
| n=NUM { number n }
| x=fvae_id { identifier x }
| LPAR e=fvae_expr RPAR { e }

fae_expr:
| e=fae_sum { e }
| VAL x=fae_id EQUAL init=fae_expr IN body=fae_expr { let_expr x init body }
| DEF f=fae_id LPAR x=fae_id RPAR EQUAL func_body=fae_expr IN body=fae_expr
    { let_expr f (function_expr x func_body) body }
| FUN x=fae_id ARROW body=fae_expr { function_expr x body }

fae_sum:
| e=fae_app { e }
| left=fae_sum PLUS right=fae_app { binary "+" left right }
| left=fae_sum MINUS right=fae_app { binary "-" left right }

fae_app:
| e=fae_atom { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=fae_app argument=fae_atom { application func argument }

fae_atom:
| n=NUM { number n }
| x=fae_id { identifier x }
| LPAR e=fae_expr RPAR { e }

rfae_expr:
| e=rfae_sum { e }
| VAL x=rfae_id EQUAL init=rfae_expr IN body=rfae_expr { let_expr x init body }
| DEF f=rfae_id LPAR x=rfae_id RPAR EQUAL func_body=rfae_expr IN body=rfae_expr
    { recursive_binding f x func_body body }
| FUN x=rfae_id ARROW body=rfae_expr { function_expr x body }
| IF0 condition=rfae_expr THEN consequent=rfae_expr ELSE alternative=rfae_expr
    { conditional condition consequent alternative }

rfae_sum:
| e=rfae_app { e }
| left=rfae_sum PLUS right=rfae_app { binary "+" left right }
| left=rfae_sum MINUS right=rfae_app { binary "-" left right }

rfae_app:
| e=rfae_atom { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=rfae_app argument=rfae_atom { application func argument }

rfae_atom:
| n=NUM { number n }
| x=rfae_id { identifier x }
| LPAR e=rfae_expr RPAR { e }

bfae_expr:
| e=bfae_seq { e }
| VAL x=bfae_id EQUAL init=bfae_expr IN body=bfae_expr { let_expr x init body }
| DEF f=bfae_id LPAR x=bfae_id RPAR EQUAL func_body=bfae_expr IN body=bfae_expr
    { let_expr f (function_expr x func_body) body }
| FUN x=bfae_id ARROW body=bfae_expr { function_expr x body }

bfae_seq:
| e=bfae_assign { e }
| first=bfae_assign SEMI second=bfae_expr { binary ";" first second }

bfae_assign:
| e=bfae_sum { e }
| left=bfae_sum ASSIGN right=bfae_assign { binary ":=" left right }

bfae_sum:
| e=bfae_app { e }
| left=bfae_sum PLUS right=bfae_app { binary "+" left right }
| left=bfae_sum MINUS right=bfae_app { binary "-" left right }

bfae_app:
| e=bfae_prefix { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=bfae_app argument=bfae_prefix { application func argument }

bfae_prefix:
| e=bfae_atom { e }
| BOX e=bfae_operand { new_box e }
| BANG e=bfae_operand { open_box e }

bfae_operand:
| e=bfae_prefix { e }
| MINUS n=NUM { number (Bigint.neg n) }

bfae_atom:
| n=NUM { number n }
| x=bfae_id { identifier x }
| LPAR e=bfae_expr RPAR { e }

mfae_expr:
| e=mfae_seq { e }
| VAL x=mfae_id EQUAL init=mfae_expr IN body=mfae_expr { let_expr x init body }
| DEF f=mfae_id LPAR x=mfae_id RPAR EQUAL func_body=mfae_expr IN body=mfae_expr
    { let_expr f (function_expr x func_body) body }
| FUN x=mfae_id ARROW body=mfae_expr { function_expr x body }

mfae_seq:
| e=mfae_assign { e }
| first=mfae_assign SEMI second=mfae_expr { sequence_sugar first second }

mfae_assign:
| e=mfae_sum { e }
| x=mfae_id ASSIGN right=mfae_assign { binary ":=" (identifier x) right }

mfae_sum:
| e=mfae_app { e }
| left=mfae_sum PLUS right=mfae_app { binary "+" left right }
| left=mfae_sum MINUS right=mfae_app { binary "-" left right }

mfae_app:
| e=mfae_atom { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=mfae_app argument=mfae_atom { application func argument }

mfae_atom:
| n=NUM { number n }
| x=mfae_id { identifier x }
| LPAR e=mfae_expr RPAR { e }

kfae_id:
| x=ID { x }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }

kfae_expr:
| e=kfae_sum { e }
| VCC x=kfae_id IN body=kfae_expr { capture x body }
| VAL x=kfae_id EQUAL init=kfae_expr IN body=kfae_expr { let_expr x init body }
| DEF f=kfae_id LPAR x=kfae_id RPAR EQUAL func_body=kfae_expr IN body=kfae_expr
    { let_expr f (function_expr x func_body) body }
| FUN x=kfae_id ARROW body=kfae_expr { function_expr x body }

kfae_sum:
| e=kfae_app { e }
| left=kfae_sum PLUS right=kfae_app { binary "+" left right }
| left=kfae_sum MINUS right=kfae_app { binary "-" left right }

kfae_app:
| e=kfae_atom { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=kfae_app argument=kfae_atom { application func argument }

kfae_atom:
| n=NUM { number n }
| x=kfae_id { identifier x }
| LPAR e=kfae_expr RPAR { e }

integer:
| n=NUM { n }
| MINUS n=NUM { Bigint.neg n }
