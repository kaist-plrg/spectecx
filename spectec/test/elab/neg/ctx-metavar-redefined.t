  $ ./main.exe ctx-metavar-redefined.spectec 2>&1
  error[elab/ctx-metavar-redefined]: meta-variable `x` was already defined
    --> ctx-metavar-redefined.spectec:4:5
    |
  4 | var x : int
    |     ^
    = source: elab
    = related: originally defined here
      --> ctx-metavar-redefined.spectec:3:5
      |
    3 | var x : nat
      |     ^
  [1]
