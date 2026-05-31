  $ ./main.exe relation-input-hint-non-hole.spectec 2>&1
  warning[elab/relation-input-hint-non-hole]: malformed input hint: should be a sequence of indexed holes %N (N < 2)
    --> relation-input-hint-non-hole.spectec:6:1
    |
  6 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
  7 |   hint(input 0)
    | ^^^^^^^^^^^^^^^
  
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> relation-input-hint-non-hole.spectec:6:1
    |
  6 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
  7 |   hint(input 0)
    | ^^^^^^^^^^^^^^^
  
  warning: hint "input" payload malformed: expected a sequence of indexed holes %N
    --> relation-input-hint-non-hole.spectec:7:8
    |
  7 |   hint(input 0)
    |        ^^^^^
    |
    | source: elab
  [1]
