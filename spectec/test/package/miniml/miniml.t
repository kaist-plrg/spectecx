Installing the Mini-ML target package adds its command and default specification.

  $ grep -q 'target_plugins/miniml/META' ../../../../spectec-target-miniml.install

  $ spectec --help | grep '^  miniml'
    miniml                     . Mini-ML commands

  $ spectec miniml parse -p ../../../testdata/interp/miniml/add.ml --color never
  + (40, 2)

Lexical errors report the line and column of the invalid character.

  $ printf 'let x = 1 in\n  @\n' > input.ml
  $ spectec miniml parse -p input.ml --color never
  error: lexical error: @
    --> input.ml:2:3
    |
  2 |   @
    |   ^
    |
    | source: task-parse
  [1]
