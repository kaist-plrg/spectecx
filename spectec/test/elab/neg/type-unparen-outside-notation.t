  $ ./main.exe type-unparen-outside-notation.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-unparen-outside-notation.spectec:4:1
    |
  4 | dec $f : nat
    | ^^^^^^^^^^^^
    = source: elab
  error: misplaced unparenthesize
    --> type-unparen-outside-notation.spectec:5:10
    |
  5 | def $f = ## 0
    |          ^^^^
    = source: elab
  [1]
