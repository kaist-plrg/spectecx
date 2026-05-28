  $ ./main.exe type-unparen-outside-hint.spectec 2>&1
  error[elab/unparen-outside-hint]: misplaced unparenthesize
    --> type-unparen-outside-hint.spectec:5:10
    |
  5 | def $f = ## 0
    |          ^^^^
    |
    | note: The `##` operator strips enclosing parentheses from its operand when a
    |       `hint(...)` expression is rendered, giving finer control over the
    |       rendered form. Outside a hint, it has no meaning.
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-unparen-outside-hint.spectec:4:1
    |
  4 | dec $f : nat
    | ^^^^^^^^^^^^
  [1]
