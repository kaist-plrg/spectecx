  $ ./main.exe type-functyp-tparam-arity-mismatch.spectec 2>&1
  error: type parameters do not match
    = source: elab
  warning: dec $g has no clauses defined
    --> type-functyp-tparam-arity-mismatch.spectec:4:1
    |
  4 | dec $g<T>(T) : T
    | ^^^^^^^^^^^^^^^^
    = source: elab
  warning: dec $caller has no clauses defined
    --> type-functyp-tparam-arity-mismatch.spectec:6:1
    |
  6 | dec $caller(def $h<U, V>(nat) : nat) : nat
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    = source: elab
  warning: dec $bad has no clauses defined
    --> type-functyp-tparam-arity-mismatch.spectec:8:1
    |
  8 | dec $bad : nat
    | ^^^^^^^^^^^^^^
    = source: elab
  [1]
