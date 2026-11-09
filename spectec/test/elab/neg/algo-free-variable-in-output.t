  $ ./main.exe algo-free-variable-in-output.spectec 2>&1
  error[algo/free-variable-in-output]: expression has free variable(s): { u : val }
    --> algo-free-variable-in-output.spectec:14:8
     |
  14 |   v |- u
     |        ^
     |
     | note: Every variable here must already be bound by an earlier part of the
     |       rule (the conclusion's input slot or a preceding premise).
  [1]
