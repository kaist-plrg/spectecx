%{
open Lang.Il.Value
open Lang.Il.Case

let number n = [tag "NUM"; arg (int n)] |> case_v ~var:"expr"
let identifier x = [tag "ID"; arg (text x)] |> case_v ~var:"id"
let binary operator left right =
  [arg left; op operator; arg right] |> case_v ~var:"expr"
let binding x init body =
  [kw "VAL"; arg (identifier x); op "="; arg init; kw "IN"; arg body]
  |> case_v ~var:"expr"
%}

%token EOF
%token <Bigint.t> NUM
%token <string> ID
%token PLUS MINUS LPAR RPAR VAL IN EQUAL

%start <Lang.Il.value> ae vae

%%

ae:
| e=ae_expr EOF { e }

vae:
| e=vae_expr EOF { e }

ae_expr:
| e=ae_atom { e }
| left=ae_expr PLUS right=ae_atom { binary "+" left right }
| left=ae_expr MINUS right=ae_atom { binary "-" left right }

ae_atom:
| n=integer { number n }
| LPAR e=ae_expr RPAR { e }

vae_expr:
| e=vae_sum { e }
| VAL x=ID EQUAL init=vae_expr IN body=vae_expr { binding x init body }

vae_sum:
| e=vae_atom { e }
| left=vae_sum PLUS right=vae_atom { binary "+" left right }
| left=vae_sum MINUS right=vae_atom { binary "-" left right }

vae_atom:
| n=integer { number n }
| x=ID { identifier x }
| LPAR e=vae_expr RPAR { e }

integer:
| n=NUM { n }
| MINUS n=NUM { Bigint.neg n }
