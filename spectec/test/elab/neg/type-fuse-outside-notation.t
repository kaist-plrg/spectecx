  $ ./main.exe type-fuse-outside-notation.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-fuse-outside-notation.spectec:4:1
    |
  4 | dec $f : nat
    | ^^^^^^^^^^^^
    = source: elab
  error: misplaced token concatenation
    --> type-fuse-outside-notation.spectec:5:10
    |
  5 | def $f = 0 # 1
    |          ^^^^^
    = source: elab
  [1]
