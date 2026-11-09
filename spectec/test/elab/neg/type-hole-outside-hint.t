  $ ./main.exe type-hole-outside-hint.spectec 2>&1
  error[elab/hole-outside-hint]: misplaced hole
    --> type-hole-outside-hint.spectec:5:10
    |
  5 | def $f = %0
    |          ^^
    |
    | note: A `%`, `%N`, `%%`, or `!%` marks an argument slot inside a `hint(...)`
    |       expression, like `hint(input %0 %1)`. Outside a hint, it has no
    |       meaning.
  [1]
