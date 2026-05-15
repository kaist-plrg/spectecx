  $ ./main.exe type-funarg-shape-mismatch-sig.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $caller has no clauses defined
    --> type-funarg-shape-mismatch-sig.spectec:7:1
    |
  7 | dec $caller(def $expected : nat) : nat
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    = source: elab
  error: function argument does not match the declared function parameter expected
    --> type-funarg-shape-mismatch-sig.spectec:8:13
    |
  8 | def $caller(def $g) = 0
    |             ^^^^^^
    = source: elab
  [1]
