  $ ./main.exe dataflow-empty-iteration-premise.spectec 2>&1
  error[elab/dataflow-empty-iter-premise]: empty iteration
    --> dataflow-empty-iteration-premise.spectec:13:7
     |
  13 |   -- (if true)*
     |       ^^^^^^^
     |
     | note: Each iteration consumes one `*` (or `?`) from a variable inside it.
     |       Here, no variable has an iteration left to consume: either the body
     |       has no variables, or every variable's `*`s have already been consumed
     |       by surrounding iterations.
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> dataflow-empty-iteration-premise.spectec:8:1
    |
  8 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
  9 |   hint(input %0)
    | ^^^^^^^^^^^^^^^^
  [1]
