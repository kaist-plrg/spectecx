impty CLI: batch run over the base corpus.

  $ SPEC=../../specs/impty/base/spec.spectec

The typechecker and evaluator each run over every .imp under the base dir; the
summary counts the expected-fail _errors_ cases as passing:

  $ spectec impty batch --spec $SPEC --batch-dir ../../testdata/interp/impty/base --color never
  typechecker: 14/14 passed, 0 failed
  evaluator: 14/14 passed, 0 failed
