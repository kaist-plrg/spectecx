impty CLI: checkpoints saved in AL and SL mode.

A checkpoint records the interpreter mode of the batch that saved it.

  $ cat > spectecx.config <<EOF
  > impty.spec = ../../specs/impty/base/spec.spectec
  > EOF
  $ mkdir corpus
  $ cp ../../testdata/interp/impty/base/hello.imp corpus/hello.imp
  $ cp ../../testdata/interp/impty/base/skip.imp corpus/skip.imp

  $ spectec impty batch --batch-dir corpus --sl --instruction-coverage.level summary --checkpoint sl.ckpt --color never > /dev/null
  $ spectec impty batch --batch-dir corpus --instruction-coverage.level summary --checkpoint al.ckpt --color never > /dev/null

Reporting a checkpoint uses its saved mode to display coverage.

  $ spectec impty checkpoint report sl.ckpt --instruction-coverage.level summary --color never | sed -n '/SL Instruction Coverage/,$p'
  === SL Instruction Coverage ===
  
  SL Instructions: 67/149 (44.97%)
  
  Uncovered SL instructions:
    lookup:
      If ((pair<K, V>* matches pattern _ :: _))

  $ spectec impty checkpoint report al.ckpt --instruction-coverage.level summary --color never | grep -A2 'Instruction Coverage'
  === AL Instruction Coverage ===
  
  AL Instructions: 72/147 attempted (48.98%), 57/147 succeeded (38.78%)


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

Merging two checkpoints from the same mode adds their coverage counts.

  $ mkdir corpus-hello corpus-skip
  $ cp corpus/hello.imp corpus-hello/hello.imp
  $ cp corpus/skip.imp corpus-skip/skip.imp
  $ spectec impty batch --batch-dir corpus-hello --instruction-coverage.level summary --checkpoint al-hello.ckpt --color never > /dev/null
  $ spectec impty batch --batch-dir corpus-skip --instruction-coverage.level summary --checkpoint al-skip.ckpt --color never > /dev/null
  $ spectec impty checkpoint merge al-hello.ckpt al-skip.ckpt --output al-merged.ckpt --color never > /dev/null
  $ spectec impty checkpoint report al-merged.ckpt --instruction-coverage.level summary --color never | grep -A2 'AL Instruction Coverage'
  === AL Instruction Coverage ===
  
  AL Instructions: 72/147 attempted (48.98%), 57/147 succeeded (38.78%)

  $ spectec impty batch --batch-dir corpus-hello --sl --instruction-coverage.level summary --checkpoint sl-hello.ckpt --color never > /dev/null
  $ spectec impty batch --batch-dir corpus-skip --sl --instruction-coverage.level summary --checkpoint sl-skip.ckpt --color never > /dev/null
  $ spectec impty checkpoint merge sl-hello.ckpt sl-skip.ckpt --output sl-merged.ckpt --color never > /dev/null
  $ spectec impty checkpoint report sl-merged.ckpt --instruction-coverage.level summary --color never | grep -A2 'SL Instruction Coverage'
  === SL Instruction Coverage ===
  
  SL Instructions: 67/149 (44.97%)
