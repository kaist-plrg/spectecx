  $ ./main.exe opaque-redefined.spectec 2>&1
  error[elab/typ-fully-redefined]: type was already defined
    --> opaque-redefined.spectec:2:8
    |
  2 | syntax state = nat
    |        ^^^^^
    |
    | related: originally defined here
    |   --> opaque-redefined.spectec:1:15
    |   |
    | 1 | opaque syntax state
    |   |               ^^^^^
  [1]
