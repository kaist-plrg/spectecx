  $ ./main.exe dataflow-multibind-dimension-mismatch.spectec 2>&1
  error[elab/dataflow-multibind-dimension-mismatch]: inconsistent dimensions for multiple bindings: (left) foo, (right) foo*
    --> dataflow-multibind-dimension-mismatch.spectec:11:4
     |
  11 |   (x, x*) |- true
     |    ^
     |
     | note: A variable can have only one inferred type per binder pattern. Here,
     |       the same variable is bound in two parallel positions at different
     |       dimensions, so the elaborator cannot pick one.
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> dataflow-multibind-dimension-mismatch.spectec:7:1
    |
  7 | relation R: (foo, foo*) |- bool
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  8 |   hint(input %0)
    | ^^^^^^^^^^^^^^^^
  [1]
