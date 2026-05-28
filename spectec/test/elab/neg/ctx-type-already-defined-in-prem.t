  $ ./main.exe ctx-type-already-defined-in-prem.spectec 2>&1
  error[elab/var-prem-type-redefined]: type already defined
    --> ctx-type-already-defined-in-prem.spectec:11:10
     |
  11 |   -- var foo : nat
     |          ^^^
     |
     | related: originally defined here
     |   --> ctx-type-already-defined-in-prem.spectec:4:8
     |   |
     | 4 | syntax foo = nat
     |   |        ^^^
  
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> ctx-type-already-defined-in-prem.spectec:6:1
    |
  6 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
  7 |   hint(input %0)
    | ^^^^^^^^^^^^^^^^
  [1]
