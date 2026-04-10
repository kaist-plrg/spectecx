  $ ./main.exe relation-missing-rules.spectec 2>&1
  warning: no input hint provided
    --> relation-missing-rules.spectec:6:1
    |
  6 | relation Foo_ok: |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^^
    = source: elab
  warning: relation Foo_ok has no rules defined
    --> relation-missing-rules.spectec:6:1
    |
  6 | relation Foo_ok: |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^^
    = source: elab
  [1]
