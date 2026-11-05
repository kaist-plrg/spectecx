  $ ./main.exe algo-multibind-dimension-mismatch.spectec 2>&1
  error[algo/multibind-dimension-mismatch]: inconsistent dimensions for multiple bindings: (left) foo, (right) foo*
    --> algo-multibind-dimension-mismatch.spectec:11:4
     |
  11 |   (x, x*) |- true
     |    ^
     |
     | note: A variable can have only one inferred type per binder pattern. Here,
     |       the same variable is bound in two parallel positions at different
     |       dimensions, so the algo pass cannot pick one.
  [1]
