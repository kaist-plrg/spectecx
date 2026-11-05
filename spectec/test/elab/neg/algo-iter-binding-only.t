  $ ./main.exe algo-iter-binding-only.spectec 2>&1
  error[algo/iter-binding-only]: cannot determine dimension of binding identifier(s) only: x let x = 0
    --> algo-iter-binding-only.spectec:13:7
     |
  13 |   -- (if x = 0)*
     |       ^^^^^^^^
     |
     | note: An iteration needs a loop variable inside it: a variable already
     |       bound outside whose values it iterates over. Here, every variable
     |       inside is newly bound rather than already bound, so there is no loop
     |       variable.
  [1]
