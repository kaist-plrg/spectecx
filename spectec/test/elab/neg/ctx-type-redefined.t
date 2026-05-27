  $ ./main.exe ctx-type-redefined.spectec 2>&1
  error[elab/ctx-type-redefined]: type `foo` was already defined
    --> ctx-type-redefined.spectec:4:8
    |
  4 | syntax foo
    |        ^^^
    = source: elab
    = related: originally defined here
      --> ctx-type-redefined.spectec:3:8
      |
    3 | syntax foo
      |        ^^^
  [1]
