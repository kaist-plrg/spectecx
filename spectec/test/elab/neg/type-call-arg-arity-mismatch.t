  $ ./main.exe type-call-arg-arity-mismatch.spectec 2>&1
  error[elab/call-arg-arity-mismatch]: arguments do not match
    --> type-call-arg-arity-mismatch.spectec:6:10
    |
  6 | def $g = $f(0)
    |          ^^^^^
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-call-arg-arity-mismatch.spectec:3:1
    |
  3 | dec $f(nat, nat) : nat
    | ^^^^^^^^^^^^^^^^^^^^^^
  warning[elab/dec-missing-clauses]: dec $g has no clauses defined
    --> type-call-arg-arity-mismatch.spectec:5:1
    |
  5 | dec $g : nat
    | ^^^^^^^^^^^^
  [1]
