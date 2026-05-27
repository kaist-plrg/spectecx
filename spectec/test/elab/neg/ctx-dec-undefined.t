  $ ./main.exe ctx-dec-undefined.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $caller has no clauses defined
    --> ctx-dec-undefined.spectec:5:1
    |
  5 | dec $caller(nat) : nat
    | ^^^^^^^^^^^^^^^^^^^^^^
    |
    | source: elab
  error[elab/ctx-dec-undefined]: dec `missing` is undefined
    --> ctx-dec-undefined.spectec:6:19
    |
  6 | def $caller(n) = $missing(n)
    |                   ^^^^^^^^
    |
    | source: elab
  [1]
