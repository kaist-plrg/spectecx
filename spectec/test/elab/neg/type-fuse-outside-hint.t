  $ ./main.exe type-fuse-outside-hint.spectec 2>&1
  error[elab/fuse-outside-hint]: misplaced token concatenation
    --> type-fuse-outside-hint.spectec:5:10
    |
  5 | def $f = 0 # 1
    |          ^^^^^
    |
    | note: The `#` operator joins two fragments without a space inside a
    |       `hint(...)` expression's rendered output, like
    |       `hint(prose %0#suffix)`. Outside a hint, it has no meaning.
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-fuse-outside-hint.spectec:4:1
    |
  4 | dec $f : nat
    | ^^^^^^^^^^^^
  [1]
