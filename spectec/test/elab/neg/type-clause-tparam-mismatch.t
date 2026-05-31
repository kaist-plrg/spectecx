  $ ./main.exe type-clause-tparam-mismatch.spectec 2>&1
  error[elab/clause-tparam-mismatch]: type parameters do not match
    --> type-clause-tparam-mismatch.spectec:4:6
    |
  4 | def $f<U, V> = 0
    |      ^^
    |
    | note: A `def $f<...> = ...` clause must repeat the type parameters from its
    |       `dec` declaration with the same count and the same names in the same
    |       order.
    |
    | related: declared here
    |   --> type-clause-tparam-mismatch.spectec:3:6
    |   |
    | 3 | dec $f<T> : nat
    |   |      ^^
  
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-clause-tparam-mismatch.spectec:3:1
    |
  3 | dec $f<T> : nat
    | ^^^^^^^^^^^^^^^
  [1]
