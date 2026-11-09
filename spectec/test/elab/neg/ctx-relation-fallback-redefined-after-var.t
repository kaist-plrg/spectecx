  $ ./main.exe ctx-relation-fallback-redefined-after-var.spectec 2>&1
  error[elab/ctx-relation-fallback-redefined]: an `otherwise` rule for relation `R` was already defined
    --> ctx-relation-fallback-redefined-after-var.spectec:14:6
     |
  14 |   -- otherwise
     |      ^^^^^^^^^
     |
     | related: originally defined here
     |   --> ctx-relation-fallback-redefined-after-var.spectec:9:6
     |   |
     | 9 |   -- otherwise
     |   |      ^^^^^^^^^
  [1]
