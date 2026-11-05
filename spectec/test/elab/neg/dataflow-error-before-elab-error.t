  $ ./main.exe dataflow-error-before-elab-error.spectec 2>&1
  error[elab/dataflow-bind-both-sides-of-equality]: cannot bind on both sides of an equality: (left) { y : nat }, (right) { z : nat }
    --> dataflow-error-before-elab-error.spectec:11:9
     |
  11 |   -- if y = z
     |         ^^^^^
     |
     | note: An `=` premise reads as a comparison when both sides are already
     |       bound, or as a binder when one side is. With new variables on both
     |       sides it fits neither.
  [1]
