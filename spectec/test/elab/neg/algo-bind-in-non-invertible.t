  $ ./main.exe algo-bind-in-non-invertible.spectec 2>&1
  error[algo/bind-in-non-invertible]: invalid binding position(s) for { x : foo } in non-invertible unary operator
    --> algo-bind-in-non-invertible.spectec:13:10
     |
  13 |   -- if -x = 5
     |          ^
     |
     | note: The algo pass assigns each variable a specific piece of the
     |       surrounding value: a tuple element, a variant case's argument, a
     |       struct field, or a list element. It does not invert operators, even
     |       when their inverse would be unique.
  [1]
