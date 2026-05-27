  $ ./main.exe dataflow-empty-iteration-expression.spectec 2>&1
  warning[elab/dec-missing-clauses]: dec $f has no clauses defined
    --> dataflow-empty-iteration-expression.spectec:6:1
    |
  6 | dec $f : foo*
    | ^^^^^^^^^^^^^
  error[elab/dataflow-empty-iter-expression]: empty iteration
    --> dataflow-empty-iteration-expression.spectec:7:10
    |
  7 | def $f = 0*
    |          ^^
    |
    | note: Each iteration consumes one `*` (or `?`) from a variable inside it. Here, no variable has an iteration left to consume: either the body has no variables, or every variable's `*`s have already been consumed by surrounding iterations.
  [1]
