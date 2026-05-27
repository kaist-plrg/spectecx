  $ ./main.exe type-latex-outside-hint.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> type-latex-outside-hint.spectec:4:1
    |
  4 | dec $f : nat
    | ^^^^^^^^^^^^
  error[elab/latex-outside-hint]: misplaced LaTeX literal
    --> type-latex-outside-hint.spectec:5:10
    |
  5 | def $f = %latex("hello")
    |          ^^^^^^^^^^^^^^^
    |
    | note: A `%latex("...")` literal embeds raw LaTeX source inside a `hint(...)` expression, for use by a LaTeX rendering backend. Outside a hint, it has no meaning.
  [1]
