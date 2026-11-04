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
  [1]
