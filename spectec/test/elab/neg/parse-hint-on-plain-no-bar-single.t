  $ ./main.exe parse-hint-on-plain-no-bar-single.spectec 2>&1
  error: hints not allowed in plain type definition
    --> parse-hint-on-plain-no-bar-single.spectec:4:14
    |
  4 | syntax foo = nat hint(blah)
    |              ^^^^^^^^^^^^^^
    = source: parse
  [1]
