  $ ./main.exe dataflow-free-variable-in-output.spectec 2>&1
  warning: relation E has no rules defined
    --> dataflow-free-variable-in-output.spectec:10:1
     |
  10 | relation E: exp |- val
     | ^^^^^^^^^^^^^^^^^^^^^^
     | ...
    = source: elab
  error: expression has free variable(s): { u : val }
    --> dataflow-free-variable-in-output.spectec:14:8
     |
  14 |   v |- u
     |        ^
    = source: elab
  [1]
