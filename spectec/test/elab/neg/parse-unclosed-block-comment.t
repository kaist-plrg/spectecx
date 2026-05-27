  $ ./main.exe parse-unclosed-block-comment.spectec 2>&1
  error[parse/unclosed-block-comment]: unclosed comment
    --> parse-unclosed-block-comment.spectec:3:1
    |
  3 | (; this never closes
    | ^^^^^^^^^^^^^^^^^^^^
    | ...
  [1]
