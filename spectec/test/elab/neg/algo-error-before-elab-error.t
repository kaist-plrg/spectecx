  $ ./main.exe algo-error-before-elab-error.spectec 2>&1
  error: elaboration of expression x = true as type bool failed
    --> algo-error-before-elab-error.spectec:12:9
     |
  12 |   -- if x = true
     |         ^^^^^^^^
     |
     | source: elab
     |
     | trace:
     | algo-error-before-elab-error.spectec:12:9-12:17:
     | cannot elaborate expression x = true as type bool
  [1]
