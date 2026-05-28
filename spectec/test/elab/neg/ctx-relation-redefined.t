  $ ./main.exe ctx-relation-redefined.spectec 2>&1
  error[elab/ctx-relation-redefined]: relation `R` was already defined
    --> ctx-relation-redefined.spectec:8:10
    |
  8 | relation R: nat |- foo
    |          ^
    |
    | related: originally defined here
    |   --> ctx-relation-redefined.spectec:5:10
    |   |
    | 5 | relation R: nat |- foo
    |   |          ^
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> ctx-relation-redefined.spectec:5:1
    |
  5 | relation R: nat |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
  6 |   hint(input %0)
    | ^^^^^^^^^^^^^^^^
  [1]
