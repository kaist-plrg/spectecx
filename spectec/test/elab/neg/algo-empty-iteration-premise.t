  $ ./main.exe algo-empty-iteration-premise.spectec 2>&1
  error[algo/empty-iter-premise]: empty iteration
    --> algo-empty-iteration-premise.spectec:13:7
     |
  13 |   -- (if true)*
     |       ^^^^^^^
     |
     | note: Each iteration consumes one `*` (or `?`) from a variable inside it.
     |       Here, no variable has an iteration left to consume: either the body
     |       has no variables, or every variable's `*`s have already been consumed
     |       by surrounding iterations.
  [1]
