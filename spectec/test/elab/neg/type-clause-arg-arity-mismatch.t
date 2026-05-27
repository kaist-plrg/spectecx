  $ ./main.exe type-clause-arg-arity-mismatch.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-clause-arg-arity-mismatch.spectec:3:1
    |
  3 | dec $f(nat) : nat
    | ^^^^^^^^^^^^^^^^^
  error[elab/clause-arg-arity-mismatch]: arguments do not match
    --> type-clause-arg-arity-mismatch.spectec:4:1
    |
  4 | def $f(x, y) = x
    | ^^^^^^^^^^^^^^^^
  [1]
