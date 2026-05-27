  $ ./main.exe dataflow-iter-dimension-mismatch.spectec 2>&1
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> dataflow-iter-dimension-mismatch.spectec:7:1
    |
  7 | relation R: foo* |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^^
    | ...
  error[elab/dataflow-iter-dimension-mismatch]: mismatched iteration dimensions for identifier `x`: expected foo*, but got foo
    --> dataflow-iter-dimension-mismatch.spectec:11:9
     |
  11 |   x* |- x
     |         ^
  [1]
