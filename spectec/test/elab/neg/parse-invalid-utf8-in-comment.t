An invalid UTF-8 byte (0xff) inside a block comment. The input is
generated at test time to keep the repo ASCII-clean.

  $ printf '(;\377;)\n' > stage.spectec
  $ ./main.exe stage.spectec 2>&1
  error[parse/invalid-utf8-in-comment]: malformed UTF-8 encoding
    --> stage.spectec:1:3
    |
  1 | (;ÿ;)
    |   ^
  [1]
