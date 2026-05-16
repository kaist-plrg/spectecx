  $ ./main.exe type-latex-outside-notation.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-latex-outside-notation.spectec:4:1
    |
  4 | dec $f : nat
    | ^^^^^^^^^^^^
    = source: elab
  error: misplaced LaTeX literal
    --> type-latex-outside-notation.spectec:5:10
    |
  5 | def $f = %latex("hello")
    |          ^^^^^^^^^^^^^^^
    = source: elab
  [1]
