  $ ./main.exe ctx-dec-redefined.spectec 2>&1
  warning: dec $f has no clauses defined
    --> ctx-dec-redefined.spectec:3:1
    |
  3 | dec $f(nat) : nat
    | ^^^^^^^^^^^^^^^^^
    = source: elab
  error: dec `f` was already defined
    --> ctx-dec-redefined.spectec:4:6
    |
  4 | dec $f(nat) : nat
    |      ^^
    = source: elab
  [1]
