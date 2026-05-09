  $ ./main.exe type-functyp-param-arity-mismatch.spectec 2>&1
  warning: dec $g has no clauses defined
    --> type-functyp-param-arity-mismatch.spectec:5:1
    |
  5 | dec $g(nat) : nat
    | ^^^^^^^^^^^^^^^^^
    = source: elab
  warning: dec $caller has no clauses defined
    --> type-functyp-param-arity-mismatch.spectec:7:1
    |
  7 | dec $caller(def $h(nat, nat) : nat) : nat
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    = source: elab
  warning: dec $bad has no clauses defined
    --> type-functyp-param-arity-mismatch.spectec:9:1
    |
  9 | dec $bad : nat
    | ^^^^^^^^^^^^^^
    = source: elab
  error: parameters do not match
    --> type-functyp-param-arity-mismatch.spectec:10:20
     |
  10 | def $bad = $caller(def $g)
     |                    ^^^^^^
    = source: elab
  [1]
