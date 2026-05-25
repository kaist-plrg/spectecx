;; A text literal contains an unrecognized escape sequence (`\q`).

"abc\qdef"
  $ ./main.exe parse-illegal-escape.spectec 2>&1
  error: illegal escape
    --> parse-illegal-escape.spectec:3:7
    |
  3 | "abc\qdef"
    |       ^
    = source: parse
  [1]
