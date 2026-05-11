  $ ./main.exe ctx-defined-dec-undefined.spectec 2>&1
  error: defined dec `missing` is undefined
    --> ctx-defined-dec-undefined.spectec:3:6
    |
  3 | def $missing(n) = n
    |      ^^^^^^^^
    = source: elab
  [1]
