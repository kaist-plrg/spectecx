
%{
open Lang.Il
open Lang.Il.Value
open Lang.Il.Case
open Common.Source

let at = region_of_positions

let v_id (s : string) : value = text s
let v_op (s : string) : value = [ kw s ] |> case_v ~var:"op"

let mk (c: string) (vl: value list) : value =
  (kw c :: List.map arg vl) |> case_v ~var:"expr"

let mk_cst at n = mk "Ecst" [int n <<<| at] <<<| at
let mk_op at o = mk "Eop" [v_op o <<<| at] <<<| at
let mk_var x = mk "Evar" [x]
let mk_app e1 e2 = mk "Eapp" [e1; e2]
let mk_let x e1 e2 = mk "Elet" [x; e1; e2]
let mk_fun x e = mk "Efun" [x; e]
let mk_pair e1 e2 = mk "Epair" [e1; e2]

%}

%token EOF
%token <Bigint.t> CST
%token <string> ID
%token ADD SUB IFZ FST SND
%token LPAR RPAR LET IN FUN
%token ARROW COMMA EQUAL

%type <Lang.Il.value> expr
%type <Lang.Il.value> prog
%start prog

%%

prog:
| e=expr EOF { e }
;

expr:
| e=atom { e }
| e=atom el=atom+
  { List.fold_left
      (fun e1 e2 -> mk_app e1 e2 <<<| over_region [e1.at; e2.at])
      e el }
| LET x=id EQUAL e1=expr IN e2=expr { mk_let x e1 e2 <<<| at $sloc }
| FUN x=id ARROW e=expr { mk_fun x e <<<| at $sloc }
;

atom:
| n=CST { mk_cst (at $sloc) n }
| x=id  { mk_var x <<<| at $sloc }
| o=op  { mk_op (at $sloc) o }
| LPAR e=expr RPAR { e }
| LPAR e1=expr COMMA e2=expr RPAR { mk_pair e1 e2 <<<| at $sloc }
;

op:
| ADD { "ADD" }
| SUB { "SUB" }
| IFZ { "IFZ" }
| FST { "FST" }
| SND { "SND" }
;

id:
| x=ID { v_id x <<<| at $sloc }
;
