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
let num_type () = [kw "NUM"] |> case_v ~var:"type"
let arrow_type param result =
  [arg param; op "->"; arg result] |> case_v ~var:"type"
let typed_function x param body =
  [kw "LAMBDA"; arg (identifier x); op ":"; arg param; op "."; arg body] |> case_v ~var:"expr"
let typed_recursive_binding f x param result func_body body =
  [kw "DEF"; arg (identifier f); lparen (); arg (identifier x); op ":"; arg param;
   rparen (); op ":"; arg result; op "="; arg func_body; kw "IN"; arg body]
  |> case_v ~var:"expr"
let type_identifier x = [tag "TID"; arg (text x)] |> case_v ~var:"tid"
let type_definition a left left_type right right_type body =
  [kw "TYPE"; arg (type_identifier a); op "="; arg (identifier left); op "@";
   arg left_type; op "+"; arg (identifier right); op "@"; arg right_type;
   kw "IN"; arg body] |> case_v ~var:"expr"
let matching subject left x left_body right y right_body =
  [arg subject; kw "MATCH"; arg (identifier left); lparen ();
   arg (identifier x); rparen (); op "->"; arg left_body; op ",";
   arg (identifier right); lparen (); arg (identifier y); rparen (); op "->";
   arg right_body] |> case_v ~var:"expr"
let forall_type a body =
  [kw "FORALL"; arg (type_identifier a); op "."; arg body] |> case_v ~var:"type"
let type_abstraction a body =
  [kw "TLAMBDA"; arg (type_identifier a); op "."; arg body] |> case_v ~var:"expr"
let type_application expr typ =
  [arg expr; lbrack (); arg typ; rbrack ()] |> case_v ~var:"expr"
let label x = [tag "LABEL"; arg (text x)] |> case_v ~var:"label"
let field kind separator name value =
  [arg (label name); op separator; arg value] |> case_v ~var:kind
let record kind field_kind fields =
  [lbrace (); arg (list (Lang.Il.Typ.var field_kind [] $ no_region) fields); rbrace ()]
  |> case_v ~var:kind
let projection expr name =
  [arg expr; op "."; arg (label name)] |> case_v ~var:"expr"
%}

%token EOF
%token <Bigint.t> NUM
%token <string> ID
%token PLUS MINUS LPAR RPAR VAL IN EQUAL DEF SEMI FUN ARROW IF0 THEN ELSE BOX BANG ASSIGN VCC
%token COLON NUMTYPE TYPE MATCH TFUN FORALL LBRACE RBRACE LBRACK RBRACK AT DOT TOPTYPE BOTTOMTYPE COMMA

%start <Lang.Il.value> ae vae f1vae fvae fae rfae bfae mfae kfae tfae trfae tvfae ptfae stfae

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

tfae:
| e=tfae_expr EOF { e }

trfae:
| e=trfae_expr EOF { e }

tvfae:
| e=tvfae_expr EOF { e }

ptfae:
| e=ptfae_expr EOF { e }

stfae:
| e=stfae_expr EOF { e }

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

common_id:
| x=ID { x }
| TOPTYPE { "top" }
| BOTTOMTYPE { "bottom" }
| NUMTYPE { "num" }
| TYPE { "type" }
| MATCH { "match" }
| TFUN { "tfun" }
| FORALL { "forall" }

vae_id:
| x=common_id { x }
| DEF { "def" }
| FUN { "fun" }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

f1vae_id:
| x=common_id { x }
| FUN { "fun" }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

fvae_id:
| x=common_id { x }
| DEF { "def" }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

fae_id:
| x=common_id { x }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

rfae_id:
| x=common_id { x }
| BOX { "box" }
| VCC { "vcc" }

bfae_id:
| x=common_id { x }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| VCC { "vcc" }

mfae_id:
| x=common_id { x }
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
| x=common_id { x }
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

typ:
| t=typ_atom { t }
| param=typ_atom ARROW result=typ { arrow_type param result }

typ_atom:
| NUMTYPE { num_type () }
| LPAR t=typ RPAR { t }

tfae_expr:
| e=tfae_sum { e }
| VAL x=fae_id EQUAL init=tfae_expr IN body=tfae_expr { binding x init body }
| FUN LPAR x=fae_id COLON param=typ RPAR ARROW body=tfae_expr
    { typed_function x param body }
| DEF f=fae_id LPAR x=fae_id COLON param=typ RPAR EQUAL func_body=tfae_expr IN body=tfae_expr
    { binding f (typed_function x param func_body) body }

tfae_sum:
| e=tfae_app { e }
| left=tfae_sum PLUS right=tfae_app { binary "+" left right }
| left=tfae_sum MINUS right=tfae_app { binary "-" left right }

tfae_app:
| e=tfae_atom { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=tfae_app argument=tfae_atom { application func argument }

tfae_atom:
| n=NUM { number n }
| x=fae_id { identifier x }
| LPAR e=tfae_expr RPAR { e }

trfae_expr:
| e=trfae_sum { e }
| VAL x=rfae_id EQUAL init=trfae_expr IN body=trfae_expr { binding x init body }
| FUN LPAR x=rfae_id COLON param=typ RPAR ARROW body=trfae_expr
    { typed_function x param body }
| DEF f=rfae_id LPAR x=rfae_id COLON param=typ RPAR COLON result=typ EQUAL func_body=trfae_expr IN body=trfae_expr
    { typed_recursive_binding f x param result func_body body }
| IF0 condition=trfae_expr THEN consequent=trfae_expr ELSE alternative=trfae_expr
    { conditional condition consequent alternative }

trfae_sum:
| e=trfae_app { e }
| left=trfae_sum PLUS right=trfae_app { binary "+" left right }
| left=trfae_sum MINUS right=trfae_app { binary "-" left right }

trfae_app:
| e=trfae_atom { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=trfae_app argument=trfae_atom { application func argument }

trfae_atom:
| n=NUM { number n }
| x=rfae_id { identifier x }
| LPAR e=trfae_expr RPAR { e }

tvfae_type_name:
| x=ID { x }
| TOPTYPE { "top" }
| BOTTOMTYPE { "bottom" }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }
| TFUN { "tfun" }
| FORALL { "forall" }

tvfae_id:
| x=tvfae_type_name { x }
| NUMTYPE { "num" }

tvfae_typ:
| t=tvfae_typ_atom { t }
| param=tvfae_typ_atom ARROW result=tvfae_typ { arrow_type param result }

tvfae_typ_atom:
| NUMTYPE { num_type () }
| a=tvfae_type_name { type_identifier a }
| LPAR t=tvfae_typ RPAR { t }

tvfae_expr:
| e=tvfae_match { e }
| VAL x=tvfae_id EQUAL init=tvfae_expr IN body=tvfae_expr { binding x init body }
| FUN LPAR x=tvfae_id COLON param=tvfae_typ RPAR ARROW body=tvfae_expr
    { typed_function x param body }
| DEF f=tvfae_id LPAR x=tvfae_id COLON param=tvfae_typ RPAR EQUAL func_body=tvfae_expr IN body=tvfae_expr
    { binding f (typed_function x param func_body) body }
| TYPE a=tvfae_type_name EQUAL left=tvfae_id AT left_type=tvfae_typ PLUS right=tvfae_id AT right_type=tvfae_typ IN body=tvfae_expr
    { type_definition a left left_type right right_type body }

tvfae_match:
| e=tvfae_sum { e }
| subject=tvfae_match MATCH LBRACE left=tvfae_id LPAR x=tvfae_id RPAR ARROW left_body=tvfae_expr SEMI right=tvfae_id LPAR y=tvfae_id RPAR ARROW right_body=tvfae_expr RBRACE
    { matching subject left x left_body right y right_body }

tvfae_sum:
| e=tvfae_app { e }
| left=tvfae_sum PLUS right=tvfae_app { binary "+" left right }
| left=tvfae_sum MINUS right=tvfae_app { binary "-" left right }

tvfae_app:
| e=tvfae_atom { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=tvfae_app argument=tvfae_atom { application func argument }

tvfae_atom:
| n=NUM { number n }
| x=tvfae_id { identifier x }
| LPAR e=tvfae_expr RPAR { e }

ptfae_type_name:
| x=ID { x }
| TOPTYPE { "top" }
| BOTTOMTYPE { "bottom" }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }
| TYPE { "type" }
| MATCH { "match" }

ptfae_id:
| x=ptfae_type_name { x }
| NUMTYPE { "num" }

ptfae_typ:
| t=ptfae_typ_atom { t }
| param=ptfae_typ_atom ARROW result=ptfae_typ { arrow_type param result }
| FORALL a=ptfae_type_name DOT body=ptfae_typ { forall_type a body }

ptfae_typ_atom:
| NUMTYPE { num_type () }
| a=ptfae_type_name { type_identifier a }
| LPAR t=ptfae_typ RPAR { t }

ptfae_expr:
| e=ptfae_sum { e }
| VAL x=ptfae_id EQUAL init=ptfae_expr IN body=ptfae_expr { binding x init body }
| FUN LPAR x=ptfae_id COLON param=ptfae_typ RPAR ARROW body=ptfae_expr
    { typed_function x param body }
| DEF f=ptfae_id LPAR x=ptfae_id COLON param=ptfae_typ RPAR EQUAL func_body=ptfae_expr IN body=ptfae_expr
    { binding f (typed_function x param func_body) body }
| TFUN a=ptfae_type_name ARROW body=ptfae_expr { type_abstraction a body }

ptfae_sum:
| e=ptfae_app { e }
| left=ptfae_sum PLUS right=ptfae_app { binary "+" left right }
| left=ptfae_sum MINUS right=ptfae_app { binary "-" left right }

ptfae_app:
| e=ptfae_atom { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=ptfae_app argument=ptfae_atom { application func argument }
| expr=ptfae_app LBRACK typ=ptfae_typ RBRACK { type_application expr typ }

ptfae_atom:
| n=NUM { number n }
| x=ptfae_id { identifier x }
| LPAR e=ptfae_expr RPAR { e }

field_label:
| x=common_id { x }
| VAL { "val" }
| IN { "in" }
| DEF { "def" }
| FUN { "fun" }
| IF0 { "if0" }
| THEN { "then" }
| ELSE { "else" }
| BOX { "box" }
| VCC { "vcc" }

stfae_typ:
| t=stfae_typ_atom { t }
| param=stfae_typ_atom ARROW result=stfae_typ { arrow_type param result }

stfae_typ_atom:
| NUMTYPE { num_type () }
| TOPTYPE { [kw "TOP"] |> case_v ~var:"type" }
| BOTTOMTYPE { [kw "BOTTOM"] |> case_v ~var:"type" }
| LPAR t=stfae_typ RPAR { t }
| LBRACE fields=separated_list(COMMA, stfae_typefield) RBRACE { record "type" "typefield" fields }

stfae_typefield:
| name=field_label COLON typ=stfae_typ { field "typefield" ":" name typ }

stfae_expr:
| e=stfae_sum { e }
| VAL x=fae_id EQUAL init=stfae_expr IN body=stfae_expr { binding x init body }
| FUN LPAR x=fae_id COLON param=stfae_typ RPAR ARROW body=stfae_expr
    { typed_function x param body }
| DEF f=fae_id LPAR x=fae_id COLON param=stfae_typ RPAR EQUAL func_body=stfae_expr IN body=stfae_expr
    { binding f (typed_function x param func_body) body }

stfae_sum:
| e=stfae_app { e }
| left=stfae_sum PLUS right=stfae_app { binary "+" left right }
| left=stfae_sum MINUS right=stfae_app { binary "-" left right }

stfae_app:
| e=stfae_postfix { e }
| MINUS n=NUM { number (Bigint.neg n) }
| func=stfae_app argument=stfae_postfix { application func argument }

stfae_postfix:
| e=stfae_atom { e }
| expr=stfae_postfix DOT name=field_label { projection expr name }

stfae_atom:
| n=NUM { number n }
| x=fae_id { identifier x }
| LPAR e=stfae_expr RPAR { e }
| LBRACE fields=separated_list(COMMA, stfae_exprfield) RBRACE { record "expr" "exprfield" fields }

stfae_exprfield:
| name=field_label EQUAL expr=stfae_expr { field "exprfield" "=" name expr }
