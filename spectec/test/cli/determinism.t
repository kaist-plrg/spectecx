impty CLI: --det, checking that each input picks a unique alternative.

  $ BASE_SPEC=../../specs/impty/base/spec.spectec
  $ OVERLAY=determinism-overlay.spectec
  $ PROG=../../testdata/interp/impty/base/hello.imp

determinism-overlay.spectec adds a second, equal-output alternative for
Check_command's decl-init case. Default execution takes the first alternative
and is unaffected:

  $ spectec impty typecheck --spec $BASE_SPEC --spec $OVERLAY -p $PROG --color never
  Typecheck succeeded

--det rejects the same run in every interpreter, including instrumented SL
execution. hello.imp declares a variable through the overlapping decl-init case:

  $ spectec impty typecheck --spec $BASE_SPEC --spec $OVERLAY -p $PROG --color never --det
  error: relation Check_command applies through both rule Check_command/decl and rule Check_command/decl-dup
    --> ../../specs/impty/base/spec.spectec:130:6
      |
  130 |   -- Check_command: tenv |- c_1 -| tenv_1
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
  [1]
  $ spectec impty typecheck --spec $BASE_SPEC --spec $OVERLAY -p $PROG --color never --det --sl --profile.output profile.json
  error: this alternative applies, and so does the alternative at ../../specs/impty/base/spec.spectec:109:26-109:40:
    --> determinism-overlay.spectec:7:26
    |
  7 |   tenv |- (t x '=' e) -| (x -> t)::tenv
    |                          ^^^^^^^^^^^^^^
    |
    | source: sl-interp
  [1]
  $ spectec impty typecheck --spec $BASE_SPEC --spec $OVERLAY -p $PROG --color never --det --pl
  error: this arm applies, and so does the arm at ../../specs/impty/base/spec.spectec:109:26-109:40:
    --> determinism-overlay.spectec:7:26
    |
  7 |   tenv |- (t x '=' e) -| (x -> t)::tenv
    |                          ^^^^^^^^^^^^^^
    |
    | source: pl-interp
  [1]

The whole-target batch path rejects the overlapping input:

  $ mkdir corpus
  $ cp $PROG corpus/hello.imp
  $ spectec impty batch --spec $BASE_SPEC --spec $OVERLAY --batch-dir corpus --color never --det
  typechecker: 0/1 passed, 1 failed
    fail             corpus/hello.imp
  evaluator: 0/1 passed, 1 failed
    fail             corpus/hello.imp

A task's --batch-dir also enables checking:

  $ spectec impty eval -p $PROG --spec $BASE_SPEC --spec $OVERLAY --batch-dir corpus --color never --det --sl
  >>> Running evaluator on corpus/hello.imp
  Failed: corpus/hello.imp
  error: this alternative applies, and so does the alternative at ../../specs/impty/base/spec.spectec:109:26-109:40:
    --> determinism-overlay.spectec:7:26
    |
  7 |   tenv |- (t x '=' e) -| (x -> t)::tenv
    |                          ^^^^^^^^^^^^^^
    |
    | source: sl-interp
  
  
  Test Results: 0/1 passed, 1 failed

A deterministic input succeeds with --det:

  $ spectec impty typecheck --spec $BASE_SPEC --spec $OVERLAY -p ../../testdata/interp/impty/base/skip.imp --color never --det
  Typecheck succeeded
