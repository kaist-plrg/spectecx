  $ ./main.exe relation-input-hint-non-hole.spectec 2>&1
  warning[elab/relation-input-hint-non-hole]: malformed input hint: should be a sequence of indexed holes %N (N < 2)
    --> relation-input-hint-non-hole.spectec:6:1
    |
  6 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> relation-input-hint-non-hole.spectec:6:1
    |
  6 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  [1]
