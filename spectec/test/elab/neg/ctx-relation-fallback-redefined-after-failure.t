  $ ./main.exe ctx-relation-fallback-redefined-after-failure.spectec 2>&1
  error: elaboration of expression true as type nat failed
    --> ctx-relation-fallback-redefined-after-failure.spectec:8:8
    |
  8 |   x |- true
    |        ^^^^
    |
    | source: elab
    |
    | trace:
    | ctx-relation-fallback-redefined-after-failure.spectec:8:8-8:12:
    | cannot cast bool to nat
  
  error[elab/ctx-relation-fallback-redefined]: an `otherwise` rule for relation `R` was already defined
    --> ctx-relation-fallback-redefined-after-failure.spectec:13:6
     |
  13 |   -- otherwise
     |      ^^^^^^^^^
     |
     | related: originally defined here
     |   --> ctx-relation-fallback-redefined-after-failure.spectec:9:6
     |   |
     | 9 |   -- otherwise
     |   |      ^^^^^^^^^
  [1]
