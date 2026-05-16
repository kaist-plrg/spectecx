  $ ./main.exe type-clause-tparam-mismatch.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-clause-tparam-mismatch.spectec:3:1
    |
  3 | dec $f<T> : nat
    | ^^^^^^^^^^^^^^^
    = source: elab
  error: type arguments do not match
    --> type-clause-tparam-mismatch.spectec:4:6
    |
  4 | def $f<U, V> = 0
    |      ^^
    = source: elab
  [1]
