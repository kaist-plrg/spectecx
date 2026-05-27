  $ ./main.exe type-funarg-expected-exp-got-fun.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $g has no clauses defined
    --> type-funarg-expected-exp-got-fun.spectec:4:1
    |
  4 | dec $g : nat
    | ^^^^^^^^^^^^
    |
    | source: elab
  warning[elab/dec-missing-clauses]: dec $caller has no clauses defined
    --> type-funarg-expected-exp-got-fun.spectec:6:1
    |
  6 | dec $caller(nat) : nat
    | ^^^^^^^^^^^^^^^^^^^^^^
    |
    | source: elab
  warning[elab/dec-missing-clauses]: dec $main has no clauses defined
    --> type-funarg-expected-exp-got-fun.spectec:8:1
    |
  8 | dec $main : nat
    | ^^^^^^^^^^^^^^^
    |
    | source: elab
  error[elab/funarg-expected-exp-got-fun]: expected an expression argument, but got a function argument
    --> type-funarg-expected-exp-got-fun.spectec:9:21
    |
  9 | def $main = $caller(def $g)
    |                     ^^^^^^
    |
    | source: elab
  [1]
