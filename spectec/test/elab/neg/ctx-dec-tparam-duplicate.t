  $ ./main.exe ctx-dec-tparam-duplicate.spectec 2>&1
  error[elab/dec-tparam-not-distinct]: type parameters are not distinct
    --> ctx-dec-tparam-duplicate.spectec:3:6
    |
  3 | dec $f<T, T> : nat
    |      ^^
    |
    | source: elab
  [1]
