  $ ./main.exe ctx-dec-undefined.spectec 2>&1
  error[elab/ctx-dec-undefined]: dec `missing` is undefined
    --> ctx-dec-undefined.spectec:6:19
    |
  6 | def $caller(n) = $missing(n)
    |                   ^^^^^^^^
  [1]
