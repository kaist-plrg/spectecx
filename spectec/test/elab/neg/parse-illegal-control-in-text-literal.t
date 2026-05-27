A text literal containing a raw control character (BEL, 0x07) instead of
the legal escape form. The input is generated at test time to keep the
repo ASCII-clean.

  $ printf '"abc\007def"\n' > stage.spectec
  $ ./main.exe stage.spectec 2>&1
  error[parse/illegal-control-in-text-literal]: illegal control character in text literal
    --> stage.spectec:1:1
    |
  1 | "abcdef"
    | ^^^^^
  [1]
