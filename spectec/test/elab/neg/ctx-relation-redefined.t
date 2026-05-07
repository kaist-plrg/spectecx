  $ ./main.exe ctx-relation-redefined.spectec 2>&1
  warning: relation R has no rules defined
    --> ctx-relation-redefined.spectec:5:1
    |
  5 | relation R: nat |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error: relation `R` was already defined
    --> ctx-relation-redefined.spectec:8:10
    |
  8 | relation R: nat |- foo
    |          ^
    = source: elab
  [1]
