  $ ./main.exe relation-negated-premise-on-output-relation.spectec 2>&1
  error[elab/negated-premise-takes-inputs]: negated rule premises do not take inputs
    --> relation-negated-premise-on-output-relation.spectec:16:10
     |
  16 |   -- R:/ 0 |- 0
     |          ^^^^^^
     |
     | note: A negated rule premise asserts that a relation does not hold for
     |       given inputs. The relation must therefore have only input positions:
     |       outputs would have no value to produce when the relation fails.
  [1]
