An invalid UTF-8 byte (0xff) where a token is expected. The input is
generated at test time to keep the repo ASCII-clean.

  $ printf '\377\n' > stage.spectec
  $ ./main.exe stage.spectec 2>&1
  error[parse/invalid-utf8]: malformed UTF-8 encoding
    --> stage.spectec:1:1
    |
  1 | ÿ
    | ^
    |
    | source: parse
  [1]
