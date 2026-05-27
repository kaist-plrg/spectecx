  $ ./main.exe parse-relation-body-must-be-notation.spectec 2>&1
  error[parse/notation-type-expected]: expected notation type
    --> parse-relation-body-must-be-notation.spectec:3:13
    |
  3 | relation R: nat
    |             ^^^
    = source: parse
    = note: A notation type includes literal tokens like `|-` or `:` that rules pattern-match against. A bare type like `nat` names a set of values without any tokens, so it cannot serve as a relation body.
  [1]
