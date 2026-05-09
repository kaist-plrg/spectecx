  $ ./main.exe parse-hint-on-plain-bar-multi.spectec 2>&1
  error: hints not allowed in plain type definition
    --> parse-hint-on-plain-bar-multi.spectec:4:14
    |
  4 | syntax foo = | nat hint(blah) | int
    |              ^^^^^^^^^^^^^^^^^^^^^^
    = source: parse
  [1]
