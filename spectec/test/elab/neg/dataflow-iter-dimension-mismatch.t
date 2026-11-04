  $ ./main.exe dataflow-iter-dimension-mismatch.spectec 2>&1
  error[elab/dataflow-iter-dimension-mismatch]: mismatched iteration dimensions for identifier `x`: expected foo*, but got foo
    --> dataflow-iter-dimension-mismatch.spectec:11:9
     |
  11 |   x* |- x
     |         ^
  [1]
