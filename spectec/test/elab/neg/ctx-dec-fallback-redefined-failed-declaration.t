  $ ./main.exe ctx-dec-fallback-redefined-failed-declaration.spectec 2>&1
  error[elab/ctx-type-undefined]: type `undefined` is undefined
    --> ctx-dec-fallback-redefined-failed-declaration.spectec:3:8
    |
  3 | dec $f(undefined) : nat
    |        ^^^^^^^^^
  
  error[elab/ctx-defined-dec-undefined]: defined dec `f` is undefined
    --> ctx-dec-fallback-redefined-failed-declaration.spectec:5:6
    |
  5 | def $f(x) = 1
    |      ^^
  
  error[elab/ctx-defined-dec-undefined]: defined dec `f` is undefined
    --> ctx-dec-fallback-redefined-failed-declaration.spectec:8:6
    |
  8 | def $f(x) = 2
    |      ^^
  [1]
