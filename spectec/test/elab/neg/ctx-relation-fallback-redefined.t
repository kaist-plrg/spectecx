  $ ./main.exe ctx-relation-fallback-redefined.spectec 2>&1
  error[elab/ctx-relation-fallback-redefined]: an `otherwise` rule for relation `R` was already defined
    --> ctx-relation-fallback-redefined.spectec:17:6
     |
  17 |   -- otherwise
     |      ^^^^^^^^^
     |
     | related: originally defined here
     |   --> ctx-relation-fallback-redefined.spectec:13:6
     |    |
     | 13 |   -- otherwise
     |    |      ^^^^^^^^^
  [1]
