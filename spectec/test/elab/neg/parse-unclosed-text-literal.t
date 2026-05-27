;; A text literal opens with `"` but reaches end of line without closing.

"abc
  $ ./main.exe parse-unclosed-text-literal.spectec 2>&1
  error[parse/unclosed-text-literal]: unclosed text literal
    --> parse-unclosed-text-literal.spectec:3:1
    |
  3 | "abc
    | ^^^^^
  [1]
