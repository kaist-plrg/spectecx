impty CLI: instruction-coverage summary in SL mode against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

The SL summary counts instruction runs and lists the instructions that never ran.

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --sl --instruction-coverage.level summary
  
  === SL Instruction Coverage ===
  
  SL Instructions: 65/149 (43.62%)
  
  Uncovered SL instructions:
    lookup:
      If ((pair<K, V>* matches pattern _ :: _))
  [
    y -> true,
    x -> 5
  ]

An SL batch reports instruction coverage across its inputs.

  $ spectec impty batch --spec $SPEC --batch-dir ../../testdata/interp/impty/base --color never --sl --instruction-coverage.level summary
  
  === SL Instruction Coverage ===
  
  SL Instructions: 149/149 (100.00%)
  typechecker: 14/14 passed, 0 failed
  evaluator: 14/14 passed, 0 failed

SL typechecking over a directory reports instruction coverage across its inputs.

  $ spectec impty typecheck --spec $SPEC -p $HELLO --batch-dir ../../testdata/interp/impty/base --color never --sl --instruction-coverage.level summary | grep -A2 'SL Instruction Coverage'
  === SL Instruction Coverage ===
  
  SL Instructions: 70/149 (46.98%)
