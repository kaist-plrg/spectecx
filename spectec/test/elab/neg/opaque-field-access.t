  $ ./main.exe opaque-field-access.spectec 2>&1
  error: elaboration of expression state.COUNT as type nat failed
    --> opaque-field-access.spectec:4:23
    |
  4 | def $invalid(state) = state.COUNT
    |                       ^^^^^^^^^^^
    |
    | source: elab
    |
    | trace:
    | opaque-field-access.spectec:4:23-4:34:
    | cannot elaborate expression state.COUNT as type nat
  [1]
