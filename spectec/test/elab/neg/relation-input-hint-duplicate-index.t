  $ ./main.exe relation-input-hint-duplicate-index.spectec 2>&1
  error: malformed input hint: inputs should be distinct
    --> relation-input-hint-duplicate-index.spectec:5:1
    |
  5 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  [1]
