  $ ./main.exe algo-after-elab-failure.spectec 2>&1
  error: elaboration of expression true as type nat failed
    --> algo-after-elab-failure.spectec:9:8
    |
  9 |   x |- true
    |        ^^^^
    |
    | source: elab
    |
    | trace:
    | algo-after-elab-failure.spectec:9:8-9:12:
    | cannot cast bool to nat
  
  error[algo/free-variable-in-output]: expression has free variable(s): { y : nat }
    --> algo-after-elab-failure.spectec:12:8
     |
  12 |   x |- y
     |        ^
     |
     | note: Every variable here must already be bound by an earlier part of the
     |       rule (the conclusion's input slot or a preceding premise).
  [1]
