  $ ./main.exe opaque-redeclared.spectec 2>&1
  error[elab/typ-fully-redefined]: type was already defined
    --> opaque-redeclared.spectec:2:15
    |
  2 | opaque syntax state
    |               ^^^^^
    |
    | related: originally defined here
    |   --> opaque-redeclared.spectec:1:15
    |   |
    | 1 | opaque syntax state
    |   |               ^^^^^
  [1]
