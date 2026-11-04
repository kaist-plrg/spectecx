  $ ./main.exe relation-iter-var-premise.spectec 2>&1
  error[elab/iter-only-rule-or-if-premise]: only rule or if premises can be iterated
    --> relation-iter-var-premise.spectec:10:7
     |
  10 |   -- (var x : foo)*
     |       ^^^^^^^^^^^
     |
     | note: Each iteration of `(prem)*` runs `prem` once. Variable declarations
     |       and `otherwise` are declared once per rule, not per iteration, so
     |       iterating them has no meaning.
  [1]
