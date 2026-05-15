  $ ./main.exe dataflow-multibind-dimension-mismatch.spectec 2>&1
  warning: relation R has no rules defined
    --> dataflow-multibind-dimension-mismatch.spectec:7:1
    |
  7 | relation R: (foo, foo*) |- bool
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error: inconsistent dimensions for multiple bindings: (left) foo, (right) foo*
    --> dataflow-multibind-dimension-mismatch.spectec:11:4
     |
  11 |   (x, x*) |- true
     |    ^
    = source: elab
  [1]
