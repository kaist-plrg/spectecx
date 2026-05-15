  $ ./main.exe relation-negated-premise-on-output-relation.spectec 2>&1
  warning: relation P has no rules defined
    --> relation-negated-premise-on-output-relation.spectec:11:1
     |
  11 | relation P: foo |- foo
     | ^^^^^^^^^^^^^^^^^^^^^^
     | ...
    = source: elab
  error: negated rule premises do not take inputs
    --> relation-negated-premise-on-output-relation.spectec:16:10
     |
  16 |   -- R:/ 0 |- 0
     |          ^^^^^^
    = source: elab
  [1]
