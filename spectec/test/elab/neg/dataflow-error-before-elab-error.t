  $ ./main.exe dataflow-error-before-elab-error.spectec 2>&1
  error: elaboration of expression x = true as type bool failed
    --> dataflow-error-before-elab-error.spectec:12:9
     |
  12 |   -- if x = true
     |         ^^^^^^^^
     |
     | source: elab
     |
     | trace:
     | dataflow-error-before-elab-error.spectec:12:9-12:17:
     | cannot elaborate expression x = true as type bool
  [1]
