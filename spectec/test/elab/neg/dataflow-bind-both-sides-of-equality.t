  $ ./main.exe dataflow-bind-both-sides-of-equality.spectec 2>&1
  error[elab/dataflow-bind-both-sides-of-equality]: cannot bind on both sides of an equality: (left) { x : foo }, (right) { y : foo }
    --> dataflow-bind-both-sides-of-equality.spectec:14:9
     |
  14 |   -- if x = y
     |         ^^^^^
     |
     | note: An `=` premise reads as a comparison when both sides are already
     |       bound, or as a binder when one side is. With new variables on both
     |       sides it fits neither.
  [1]
