  $ ./main.exe parse-hole-index-overflow.spectec 2>&1
  error[parse/hole-index-overflow]: hole index out of range
    --> parse-hole-index-overflow.spectec:6:14
    |
  6 |   hint(input %99999999999999999999)
    |              ^^^^^^^^^^^^^^^^^^^^^
    |
    | note: The hole index in `%N` must fit in a 63-bit native integer.
  [1]
