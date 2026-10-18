  $ ./main.exe opaque-type-mismatch.spectec 2>&1
  error: elaboration of expression otherState as type state failed
    --> opaque-type-mismatch.spectec:6:34
    |
  6 | def $invalid(otherState) = $read(otherState)
    |                                  ^^^^^^^^^^
    |
    | source: elab
    |
    | trace:
    | opaque-type-mismatch.spectec:6:34-6:44:
    | cannot cast otherState to state
  [1]
