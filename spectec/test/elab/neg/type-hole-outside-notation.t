  $ ./main.exe type-hole-outside-notation.spectec 2>&1
  warning: dec $f has no clauses defined
    --> type-hole-outside-notation.spectec:4:1
    |
  4 | dec $f : nat
    | ^^^^^^^^^^^^
    = source: elab
  error: misplaced hole
    --> type-hole-outside-notation.spectec:5:10
    |
  5 | def $f = %0
    |          ^^
    = source: elab
  [1]
