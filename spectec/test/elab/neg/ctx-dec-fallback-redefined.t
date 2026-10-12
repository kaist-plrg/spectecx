  $ ./main.exe ctx-dec-fallback-redefined.spectec 2>&1
  error[elab/ctx-dec-fallback-redefined]: an `otherwise` clause for dec `f` was already defined
    --> ctx-dec-fallback-redefined.spectec:12:6
     |
  12 |   -- otherwise
     |      ^^^^^^^^^
     |
     | related: originally defined here
     |   --> ctx-dec-fallback-redefined.spectec:9:6
     |   |
     | 9 |   -- otherwise
     |   |      ^^^^^^^^^
  [1]
