  $ ./main.exe type-clause-arg-arity-mismatch.spectec 2>&1
  warning: dec $f has no clauses defined
    --> type-clause-arg-arity-mismatch.spectec:3:1
    |
  3 | dec $f(nat) : nat
    | ^^^^^^^^^^^^^^^^^
    = source: elab
  error: arguments do not match
    --> type-clause-arg-arity-mismatch.spectec:4:1
    |
  4 | def $f(x, y) = x
    | ^^^^^^^^^^^^^^^^
    = source: elab
  [1]
