  $ ./main.exe ctx-relation-undefined-in-premise.spectec 2>&1
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> ctx-relation-undefined-in-premise.spectec:5:1
    |
  5 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
  error[elab/ctx-relation-undefined]: relation `Missing` is undefined
    --> ctx-relation-undefined-in-premise.spectec:10:6
     |
  10 |   -- Missing: 0 |- 0
     |      ^^^^^^^
  [1]
