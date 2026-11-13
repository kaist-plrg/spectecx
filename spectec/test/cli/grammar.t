spectec grammar on a spec whose only error comes from the algo pass.

The grammar command stops after elaboration, so algo errors do not prevent it
from printing the grammar.

  $ cat > spec.spectec <<'SPEC'
  > syntax expr = NUM nat | ADD expr expr
  > 
  > var n : nat
  > var m : nat
  > 
  > relation Eval: expr |- nat
  >   hint(input %0)
  > 
  > rule Eval/num:
  >   NUM n |- m
  > SPEC

The algo command rejects the rule because the rule never binds its output m.

  $ spectec algo spec.spectec --color never 2>&1 | head -1
  error[algo/free-variable-in-output]: expression has free variable(s): { m : nat }

  $ spectec grammar --start expr spec.spectec --color never
  syntax expr:
    | NUM <nat>                                    neither
    | ADD <expr> <expr>                            right  
