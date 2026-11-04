  $ ./main.exe type-functyp-tparam-arity-mismatch.spectec 2>&1
  error[elab/functyp-tparam-arity-mismatch]: type parameters do not match
  
  warning[elab/dec-missing-clauses]: dec $g has no clauses defined
    --> type-functyp-tparam-arity-mismatch.spectec:4:1
    |
  4 | dec $g<T>(T) : T
    | ^^^^^^^^^^^^^^^^
  
  warning[elab/dec-missing-clauses]: dec $caller has no clauses defined
    --> type-functyp-tparam-arity-mismatch.spectec:6:1
    |
  6 | dec $caller(def $h<U, V>(nat) : nat) : nat
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]
