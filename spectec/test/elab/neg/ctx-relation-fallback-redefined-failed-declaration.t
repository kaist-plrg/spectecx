  $ ./main.exe ctx-relation-fallback-redefined-failed-declaration.spectec 2>&1
  error[elab/relation-input-hint-duplicate-index]: malformed input hint: inputs should be distinct
    --> ctx-relation-fallback-redefined-failed-declaration.spectec:3:1
    |
  3 | relation R:
    | ^^^^^^^^^^^
  4 |   nat |- nat
    | ^^^^^^^^^^^^
  5 |   hint(input %0 %0)
    | ^^^^^^^^^^^^^^^^^^^
  
  error[elab/ctx-relation-undefined]: relation `R` is undefined
    --> ctx-relation-fallback-redefined-failed-declaration.spectec:7:6
    |
  7 | rule R/first:
    |      ^
  
  error[elab/ctx-relation-undefined]: relation `R` is undefined
    --> ctx-relation-fallback-redefined-failed-declaration.spectec:11:6
     |
  11 | rule R/second:
     |      ^
  [1]
