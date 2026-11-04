  $ ./main.exe ctx-dec-fallback-redefined-after-failure.spectec 2>&1
  error: elaboration of expression true as type nat failed
    --> ctx-dec-fallback-redefined-after-failure.spectec:5:13
    |
  5 | def $f(x) = true
    |             ^^^^
    |
    | source: elab
    |
    | trace:
    | ctx-dec-fallback-redefined-after-failure.spectec:5:13-5:17:
    | cannot cast bool to nat
  
  error[elab/ctx-dec-fallback-redefined]: an `otherwise` clause for dec `f` was already defined
    --> ctx-dec-fallback-redefined-after-failure.spectec:9:6
    |
  9 |   -- otherwise
    |      ^^^^^^^^^
    |
    | related: originally defined here
    |   --> ctx-dec-fallback-redefined-after-failure.spectec:6:6
    |   |
    | 6 |   -- otherwise
    |   |      ^^^^^^^^^
  [1]
