  $ ./main.exe type-funarg-shape-mismatch-call.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $h has no clauses defined
    --> type-funarg-shape-mismatch-call.spectec:4:1
    |
  4 | dec $h(text) : nat
    | ^^^^^^^^^^^^^^^^^^
    = source: elab
  warning[elab/dec-missing-clauses]: dec $caller has no clauses defined
    --> type-funarg-shape-mismatch-call.spectec:6:1
    |
  6 | dec $caller(def $f(nat) : nat) : nat
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    = source: elab
  warning[elab/dec-missing-clauses]: dec $main has no clauses defined
    --> type-funarg-shape-mismatch-call.spectec:8:1
    |
  8 | dec $main : nat
    | ^^^^^^^^^^^^^^^
    = source: elab
  error: function argument does not match the declared function parameter f
    --> type-funarg-shape-mismatch-call.spectec:9:21
    |
  9 | def $main = $caller(def $h)
    |                     ^^^^^^
    = source: elab
  [1]
