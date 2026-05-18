  $ ./main.exe dataflow-free-variable-in-output.spectec 2>&1
  warning[elab/relation-missing-rules]: relation E has no rules defined
    --> dataflow-free-variable-in-output.spectec:10:1
     |
  10 | relation E: exp |- val
     | ^^^^^^^^^^^^^^^^^^^^^^
     | ...
    = source: elab
  error[elab/dataflow-free-variable-in-output]: expression has free variable(s): { u : val }
    --> dataflow-free-variable-in-output.spectec:14:8
     |
  14 |   v |- u
     |        ^
    = source: elab
    = note: Every variable here must already be bound by an earlier part of the rule (the conclusion's input slot or a preceding premise).
  [1]
