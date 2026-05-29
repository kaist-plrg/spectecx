  $ ./main.exe ctx-builtin-dec-tparam-duplicate.spectec 2>&1
  error[elab/builtin-dec-tparam-not-distinct]: type parameters are not distinct
    --> ctx-builtin-dec-tparam-duplicate.spectec:4:14
    |
  4 | builtin dec $f<T, T> : nat
    |              ^^
  [1]
