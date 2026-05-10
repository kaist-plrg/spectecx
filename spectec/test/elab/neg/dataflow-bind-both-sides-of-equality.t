  $ ./main.exe dataflow-bind-both-sides-of-equality.spectec 2>&1
  warning: relation R has no rules defined
    --> dataflow-bind-both-sides-of-equality.spectec:9:1
    |
  9 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error: cannot bind on both sides of an equality: (left) { x : foo }, (right) { y : foo }
    --> dataflow-bind-both-sides-of-equality.spectec:14:9
     |
  14 |   -- if x = y
     |         ^^^^^
    = source: elab
  [1]
