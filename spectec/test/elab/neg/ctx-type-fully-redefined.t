  $ ./main.exe ctx-type-fully-redefined.spectec 2>&1
  error[elab/typ-fully-redefined]: type was already defined
    --> ctx-type-fully-redefined.spectec:4:8
    |
  4 | syntax foo = int
    |        ^^^
    = source: elab
    = related: originally defined here
      --> ctx-type-fully-redefined.spectec:3:8
      |
    3 | syntax foo = nat
      |        ^^^
  [1]
