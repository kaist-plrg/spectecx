  $ ./main.exe ctx-type-already-defined-in-prem.spectec 2>&1
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> ctx-type-already-defined-in-prem.spectec:6:1
    |
  6 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error: type already defined
    --> ctx-type-already-defined-in-prem.spectec:11:10
     |
  11 |   -- var foo : nat
     |          ^^^
    = source: elab
  [1]
