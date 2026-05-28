  $ ./main.exe type-funarg-expected-fun-got-exp.spectec 2>&1
  error[elab/funarg-expected-fun-got-exp]: expected a function argument, but got an expression argument
    --> type-funarg-expected-fun-got-exp.spectec:8:21
    |
  8 | def $main = $caller(0)
    |                     ^
  
  warning[elab/dec-missing-clauses]: dec $main has no clauses defined
    --> type-funarg-expected-fun-got-exp.spectec:7:1
    |
  7 | dec $main : nat
    | ^^^^^^^^^^^^^^^
  [1]
