  $ ./main.exe type-hole-outside-hint.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-hole-outside-hint.spectec:4:1
    |
  4 | dec $f : nat
    | ^^^^^^^^^^^^
  error[elab/hole-outside-hint]: misplaced hole
    --> type-hole-outside-hint.spectec:5:10
    |
  5 | def $f = %0
    |          ^^
    |
    | note: A `%`, `%N`, `%%`, or `!%` marks an argument slot inside a `hint(...)` expression, like `hint(input %0 %1)`. Outside a hint, it has no meaning.
  [1]
