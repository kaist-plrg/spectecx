  $ ./main.exe relation-iter-var-premise.spectec 2>&1
  warning: relation R has no rules defined
    --> relation-iter-var-premise.spectec:5:1
    |
  5 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error: only rule or if premises can be iterated
    --> relation-iter-var-premise.spectec:10:7
     |
  10 |   -- (var x : foo)*
     |       ^^^^^^^^^^^
    = source: elab
  [1]
