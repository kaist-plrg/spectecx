  $ ./main.exe dataflow-empty-iteration-expression.spectec 2>&1
  warning: dec $f has no clauses defined
    --> dataflow-empty-iteration-expression.spectec:6:1
    |
  6 | dec $f : foo*
    | ^^^^^^^^^^^^^
    = source: elab
  error: empty iteration
    --> dataflow-empty-iteration-expression.spectec:7:10
    |
  7 | def $f = 0*
    |          ^^
    = source: elab
  [1]
