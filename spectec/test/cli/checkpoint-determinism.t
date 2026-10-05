impty CLI: a checkpoint records whether its run checked determinism, and
resuming or merging across a mismatched setting is rejected.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ mkdir corpus
  $ cp ../../testdata/interp/impty/base/hello.imp corpus/hello.imp
  $ cp ../../testdata/interp/impty/base/skip.imp corpus/skip.imp

A checkpoint saved without --det resumes under the same setting. Completed inputs are skipped:

  $ spectec impty batch --spec $SPEC --batch-dir corpus --checkpoint off.ckpt --color never
  typechecker: 2/2 passed, 0 failed
  evaluator: 2/2 passed, 0 failed
  $ spectec impty batch --spec $SPEC --batch-dir corpus --resume off.ckpt --checkpoint off.ckpt --color never
  typechecker: 0/0 passed, 0 failed
  evaluator: 0/0 passed, 0 failed

The same holds for a checkpoint saved with --det:

  $ spectec impty batch --spec $SPEC --batch-dir corpus --det --checkpoint on.ckpt --color never
  typechecker: 2/2 passed, 0 failed
  evaluator: 2/2 passed, 0 failed
  $ spectec impty batch --spec $SPEC --batch-dir corpus --det --resume on.ckpt --checkpoint on.ckpt --color never
  typechecker: 0/0 passed, 0 failed
  evaluator: 0/0 passed, 0 failed

Resuming with a different determinism setting is rejected in both directions:

  $ spectec impty batch --spec $SPEC --batch-dir corpus --det --resume off.ckpt --color never
  error: Checkpoint determinism checking is disabled. Resume with the same setting.
  
    source: config
  [1]

  $ spectec impty batch --spec $SPEC --batch-dir corpus --resume on.ckpt --color never
  error: Checkpoint determinism checking is enabled. Resume with the same setting.
  
    source: config
  [1]


Merging two checkpoints saved under the same setting succeeds:

  $ cp off.ckpt off-copy.ckpt
  $ spectec impty checkpoint merge off.ckpt off-copy.ckpt --output merged.ckpt --color never
  Merged checkpoint saved to: merged.ckpt
    Checkpoint 1: 4 tests
    Checkpoint 2: 4 tests
    Merged: 2 tests

Merging checkpoints saved under different settings is rejected:

  $ spectec impty checkpoint merge off.ckpt on.ckpt --output mixed.ckpt --color never
  error: Cannot merge checkpoints with different determinism settings
  
    source: config
  [1]
