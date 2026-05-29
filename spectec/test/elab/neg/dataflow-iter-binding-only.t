  $ ./main.exe dataflow-iter-binding-only.spectec 2>&1
  error[elab/dataflow-iter-binding-only]: cannot determine dimension of binding identifier(s) only: x let x = 0
    --> dataflow-iter-binding-only.spectec:13:7
     |
  13 |   -- (if x = 0)*
     |       ^^^^^^^^
     |
     | note: An iteration needs a loop variable inside it: a variable already
     |       bound outside whose values it iterates over. Here, every variable
     |       inside is newly bound rather than already bound, so there is no loop
     |       variable.
  
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> dataflow-iter-binding-only.spectec:8:1
    |
  8 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
  9 |   hint(input %0)
    | ^^^^^^^^^^^^^^^^
  [1]
