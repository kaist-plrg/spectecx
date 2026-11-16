impty CLI: checkpoints saved in AL and SL mode.

A checkpoint records the interpreter mode of the batch that saved it.

  $ cat > spectecx.config <<EOF
  > impty.spec = ../../specs/impty/base/spec.spectec
  > EOF
  $ mkdir corpus
  $ cp ../../testdata/interp/impty/base/hello.imp corpus/hello.imp
  $ cp ../../testdata/interp/impty/base/skip.imp corpus/skip.imp

  $ spectec impty batch --batch-dir corpus --sl --instruction-coverage.level summary --checkpoint sl.ckpt --color never > /dev/null
  $ spectec impty batch --batch-dir corpus --checkpoint al.ckpt --color never
  typechecker: 2/2 passed, 0 failed
  evaluator: 2/2 passed, 0 failed

Reporting a checkpoint uses its saved mode to display coverage.

  $ spectec impty checkpoint report sl.ckpt --instruction-coverage.level summary --color never | sed -n '/SL Node Coverage/,$p'
  === SL Node Coverage ===
  
  SL Instructions: 67/149 (44.97%)
  
  Uncovered SL instructions:
    lookup:
      If ((pair<K, V>* matches pattern _ :: _))


Resuming in another mode would skip inputs that ran only in the original mode, so the command rejects it.

  $ spectec impty batch --batch-dir corpus --resume sl.ckpt --color never
  error: Checkpoint was saved in SL mode. Resume in the same mode.
  
    source: config
  [1]


Both checkpoints must come from the same interpreter mode.

  $ spectec impty checkpoint merge al.ckpt sl.ckpt --output mixed.ckpt --color never
  error: Cannot merge a checkpoint saved in AL mode with one saved in SL mode
  
    source: config
  [1]

