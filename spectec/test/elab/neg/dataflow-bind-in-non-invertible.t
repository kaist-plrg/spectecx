  $ ./main.exe dataflow-bind-in-non-invertible.spectec 2>&1
  error[elab/dataflow-bind-in-non-invertible]: invalid binding position(s) for { x : foo } in non-invertible unary operator
    --> dataflow-bind-in-non-invertible.spectec:13:10
     |
  13 |   -- if -x = 5
     |          ^
     |
     | note: The elaborator assigns each variable a specific piece of the
     |       surrounding value: a tuple element, a variant case's argument, a
     |       struct field, or a list element. It does not invert operators, even
     |       when their inverse would be unique.
  warning[elab/relation-missing-rules]: relation R has no rules defined
    --> dataflow-bind-in-non-invertible.spectec:8:1
    |
  8 | relation R: foo |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^
  9 |   hint(input %0)
    | ^^^^^^^^^^^^^^^^
  [1]
