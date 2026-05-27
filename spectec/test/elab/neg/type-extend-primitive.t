  $ ./main.exe type-extend-primitive.spectec 2>&1
  error[elab/extend-non-variant-primitive]: cannot extend a non-variant type
    --> type-extend-primitive.spectec:2:5
    |
  2 |   | bool
    |     ^^^^
    |
    | note: A case-line `| T` extends the surrounding variant with the cases of `T`. The expression here is a primitive type, not a named variant, so there are no cases to contribute.
  [1]
