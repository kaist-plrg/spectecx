  $ ./main.exe parse-hint-on-plain-bar-multi.spectec 2>&1
  error[parse/hint-on-plain-bar-multi]: hints not allowed in plain type definition
    --> parse-hint-on-plain-bar-multi.spectec:4:14
    |
  4 | syntax foo = | nat hint(blah) | int
    |              ^^^^^^^^^^^^^^^^^^^^^^
    |
    | note: A plain typdef aliases an existing type, like `syntax x = nat`. It inherits hints from the aliased type and cannot carry hints of its own.
  [1]
