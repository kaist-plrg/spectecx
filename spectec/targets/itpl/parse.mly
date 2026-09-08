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
%}

%token EOF
%token <Bigint.t> NUM
%token <string> ID
%token PLUS MINUS LPAR RPAR VAL IN EQUAL DEF SEMI FUN ARROW

%start <Lang.Il.value> ae vae f1vae fvae

%%

ae:
| e=ae_expr EOF { e }

vae:
| e=vae_expr EOF { e }

f1vae:
| bindings=fbinding* e=f1vae_expr EOF { program bindings e }

fvae:
| e=fvae_expr EOF { e }

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

f1vae_id:
| x=ID { x }
| FUN { "fun" }

fvae_id:
| x=ID { x }
| DEF { "def" }

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

integer:
| n=NUM { n }
| MINUS n=NUM { Bigint.neg n }
