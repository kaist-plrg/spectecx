  $ ./main.exe type-forward-tparam-mismatch.spectec 2>&1
  error[elab/typ-tparam-mismatch]: type parameters do not match
    --> type-forward-tparam-mismatch.spectec:5:8
    |
  5 | syntax foo<T> = T
    |        ^^^^
    = source: elab
    = note: A `syntax T<...> = ...` body must repeat the type parameters from its forward declaration with the same count and the same names in the same order.
    = related: forward-declared here
    --> type-forward-tparam-mismatch.spectec:4:8
    |
  4 | syntax foo
    |        ^^^
  [1]
