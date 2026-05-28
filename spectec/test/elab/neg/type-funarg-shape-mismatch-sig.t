  $ ./main.exe type-funarg-shape-mismatch-sig.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $caller has no clauses defined
    --> type-funarg-shape-mismatch-sig.spectec:7:1
    |
  7 | dec $caller(def $expected : nat) : nat
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  error[elab/funarg-shape-mismatch-sig]: function argument does not match the declared function parameter expected
    --> type-funarg-shape-mismatch-sig.spectec:8:13
    |
  8 | def $caller(def $g) = 0
    |             ^^^^^^
    |
    | note: A function argument in a `def` clause must bind to the same name as
    |       the declared function parameter in the `dec`. The clause body uses
    |       that name to call the function.
    | related: declared here
    |   --> type-funarg-shape-mismatch-sig.spectec:7:18
    |   |
    | 7 | dec $caller(def $expected : nat) : nat
    |   |                  ^^^^^^^^
  [1]
