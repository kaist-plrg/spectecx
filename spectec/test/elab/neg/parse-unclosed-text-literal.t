;; A text literal opens with `"` but reaches end of line without closing.

"abc
  $ ./main.exe parse-unclosed-text-literal.spectec 2>&1
  error: unclosed text literal
    --> parse-unclosed-text-literal.spectec:3:1
    |
  3 | "abc
    | ^^^^^
    = source: parse
  [1]
