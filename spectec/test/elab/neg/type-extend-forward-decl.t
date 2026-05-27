  $ ./main.exe type-extend-forward-decl.spectec 2>&1
  error[elab/extend-incomplete]: cannot extend an incomplete type
    --> type-extend-forward-decl.spectec:7:5
    |
  7 |   | incomplete
    |     ^^^^^^^^^^
    = source: elab
    = note: A case-line `| T` extends the surrounding variant with the cases of `T`. The type named here was declared with `syntax T;` but has no body yet, so there are no cases to contribute.
    = related: originally declared here
      --> type-extend-forward-decl.spectec:4:8
      |
    4 | syntax incomplete
      |        ^^^^^^^^^^
  [1]
