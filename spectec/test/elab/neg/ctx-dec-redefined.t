  $ ./main.exe ctx-dec-redefined.spectec 2>&1
  error[elab/ctx-dec-redefined]: dec `f` was already defined
    --> ctx-dec-redefined.spectec:4:6
    |
  4 | dec $f(nat) : nat
    |      ^^
    |
    | related: originally defined here
    |   --> ctx-dec-redefined.spectec:3:6
    |   |
    | 3 | dec $f(nat) : nat
    |   |      ^^
  
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> ctx-dec-redefined.spectec:3:1
    |
  3 | dec $f(nat) : nat
    | ^^^^^^^^^^^^^^^^^
  [1]
