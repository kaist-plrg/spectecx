  $ ./main.exe dataflow-empty-iteration-premise.spectec 2>&1
  warning: relation R has no rules defined
    --> dataflow-empty-iteration-premise.spectec:8:1
    |
  8 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error: empty iteration
    --> dataflow-empty-iteration-premise.spectec:13:7
     |
  13 |   -- (if true)*
     |       ^^^^^^^
    = source: elab
  [1]
