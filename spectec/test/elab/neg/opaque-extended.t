  $ ./main.exe opaque-extended.spectec 2>&1
  error[elab/extend-non-variant-struct]: cannot extend a non-variant type
    --> opaque-extended.spectec:3:5
    |
  3 |   | state
    |     ^^^^^
    |
    | note: A case-line `| T` extends the surrounding variant with the cases of
    |       `T`. The type named here has a body that is not a variant, so it has
    |       no cases to contribute.
    |
    | related: originally defined here
    |   --> opaque-extended.spectec:1:15
    |   |
    | 1 | opaque syntax state
    |   |               ^^^^^
  [1]
