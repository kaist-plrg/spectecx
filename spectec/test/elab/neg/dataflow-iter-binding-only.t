  $ ./main.exe dataflow-iter-binding-only.spectec 2>&1
  warning: relation R has no rules defined
    --> dataflow-iter-binding-only.spectec:8:1
    |
  8 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error: cannot determine dimension of binding identifier(s) only: x let x = 0
    --> dataflow-iter-binding-only.spectec:13:7
     |
  13 |   -- (if x = 0)*
     |       ^^^^^^^^
    = source: elab
  [1]
