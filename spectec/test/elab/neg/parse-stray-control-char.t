A raw ASCII control character (BEL, 0x07) where a token is expected. The
input is generated at test time to keep the repo ASCII-clean.

  $ printf '\007\n' > stage.spectec
  $ ./main.exe stage.spectec 2>&1
  error[parse/stray-control-char]: misplaced control character
    --> stage.spectec:1:1
    |
  1 | 
    | ^
  [1]
