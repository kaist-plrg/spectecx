  $ ./main.exe ctx-type-already-defined-in-prem.spectec 2>&1
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> ctx-type-already-defined-in-prem.spectec:6:1
    |
  6 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
    | ...
    = source: elab
  error[elab/var-prem-type-redefined]: type already defined
    --> ctx-type-already-defined-in-prem.spectec:11:10
     |
  11 |   -- var foo : nat
     |          ^^^
    = source: elab
    = related: originally defined here
    --> ctx-type-already-defined-in-prem.spectec:4:8
    |
  4 | syntax foo = nat
    |        ^^^
  [1]
