  $ ./main.exe type-extend-struct.spectec 2>&1
  error[elab/extend-non-variant-struct]: cannot extend a non-variant type
    --> type-extend-struct.spectec:7:5
    |
  7 |   | myStruct
    |     ^^^^^^^^
    |
    | source: elab
    | note: A case-line `| T` extends the surrounding variant with the cases of `T`. The type named here has a body that is not a variant, so it has no cases to contribute.
    | related: originally defined here
    |   --> type-extend-struct.spectec:4:8
    |   |
    | 4 | syntax myStruct = { X nat }
    |   |        ^^^^^^^^
  [1]
