  $ ./main.exe dataflow-bind-in-non-invertible.spectec 2>&1
  warning: relation R has no rules defined
    --> dataflow-bind-in-non-invertible.spectec:8:1
    |
  8 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error: invalid binding position(s) for { x : foo } in non-invertible unary operator
    --> dataflow-bind-in-non-invertible.spectec:13:10
     |
  13 |   -- if -x = 5
     |          ^
    = source: elab
  [1]
