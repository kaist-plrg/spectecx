  $ ./main.exe ctx-builtin-dec-redefined.spectec 2>&1
  error[elab/ctx-builtin-dec-redefined]: dec `f` was already defined
    --> ctx-builtin-dec-redefined.spectec:4:14
    |
  4 | builtin dec $f : nat
    |              ^
    = source: elab
    = related: originally defined here
    --> ctx-builtin-dec-redefined.spectec:3:14
    |
  3 | builtin dec $f : nat
    |              ^
  [1]
