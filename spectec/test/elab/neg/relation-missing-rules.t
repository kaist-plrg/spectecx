  $ ./main.exe relation-missing-rules.spectec 2>&1
  warning[elab/relation-no-input-hint]: no input hint provided
    --> relation-missing-rules.spectec:6:1
    |
  6 | relation Foo_ok: |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^^
  
  warning[elab/relation-missing-rules]: relation Foo_ok has no rules defined
    --> relation-missing-rules.spectec:6:1
    |
  6 | relation Foo_ok: |- foo
    | ^^^^^^^^^^^^^^^^^^^^^^^
  [1]
