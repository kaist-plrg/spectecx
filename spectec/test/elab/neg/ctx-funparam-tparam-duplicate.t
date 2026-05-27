  $ ./main.exe ctx-funparam-tparam-duplicate.spectec 2>&1
  error[elab/funparam-tparam-not-distinct]: type parameters are not distinct
    --> ctx-funparam-tparam-duplicate.spectec:4:13
    |
  4 | dec $f(def $g<T, T> : nat) : nat
    |             ^^
    |
    | source: elab
  [1]
