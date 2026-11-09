  $ ./main.exe type-funarg-expected-fun-got-exp.spectec 2>&1
  error[elab/funarg-expected-fun-got-exp]: expected a function argument, but got an expression argument
    --> type-funarg-expected-fun-got-exp.spectec:8:21
    |
  8 | def $main = $caller(0)
    |                     ^
  [1]
