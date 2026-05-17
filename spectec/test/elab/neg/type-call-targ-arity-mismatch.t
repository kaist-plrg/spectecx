  $ ./main.exe type-call-targ-arity-mismatch.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-call-targ-arity-mismatch.spectec:3:1
    |
  3 | dec $f<T>(T) : T
    | ^^^^^^^^^^^^^^^^
    = source: elab
  warning[elab/dec-missing-clauses]: dec $g has no clauses defined
    --> type-call-targ-arity-mismatch.spectec:5:1
    |
  5 | dec $g : nat
    | ^^^^^^^^^^^^
    = source: elab
  error[elab/call-targ-arity-mismatch]: type arguments do not match
    --> type-call-targ-arity-mismatch.spectec:6:11
    |
  6 | def $g = $f<nat, nat>(0)
    |           ^^
    = source: elab
  [1]
