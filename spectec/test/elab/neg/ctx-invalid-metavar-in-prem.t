  $ ./main.exe ctx-invalid-metavar-in-prem.spectec 2>&1
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> ctx-invalid-metavar-in-prem.spectec:6:1
    |
  6 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error: invalid meta-variable identifier
    --> ctx-invalid-metavar-in-prem.spectec:11:10
     |
  11 |   -- var x_1 : foo
     |          ^^^
    = source: elab
  [1]
