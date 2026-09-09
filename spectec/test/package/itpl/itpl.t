The ITPL package owns its plugins and their separate default specifications.

  $ grep -q 'target_plugins/ae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/vae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/f1vae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/fvae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/fae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/rfae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/bfae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/mfae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/lfae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/kfae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/tfae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/trfae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/tvfae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'ae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'vae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'f1vae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'fvae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'fae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'rfae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'bfae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'mfae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'lfae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'kfae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'tfae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'trfae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'tvfae/spec.spectec' ../../../../spectec-target-itpl.install

  $ spectec --help | grep -E '^  (ae|vae|f1vae|fvae|fae|rfae|bfae|mfae|lfae|kfae|tfae|trfae|tvfae) '
    ae                         . AE commands
    bfae                       . BFAE commands
    f1vae                      . F1VAE commands
    fae                        . FAE commands
    fvae                       . FVAE commands
    kfae                       . KFAE commands
    lfae                       . LFAE commands
    mfae                       . MFAE commands
    rfae                       . RFAE commands
    tfae                       . TFAE commands
    trfae                      . TRFAE commands
    tvfae                      . TVFAE commands
    vae                        . VAE commands

The commands use the installed specifications without a --spec override.

  $ spectec ae eval -p ../../../testdata/interp/itpl/ae/arithmetic.ae --color never
  4
  $ spectec vae eval -p ../../../testdata/interp/itpl/vae/shadowing.vae --color never
  21
  $ spectec f1vae eval -p ../../../testdata/interp/itpl/f1vae/twice.f1vae --color never
  16
  $ spectec fvae eval -p ../../../testdata/interp/itpl/fvae/capture.fvae --color never
  11
  $ spectec fae eval -p ../../../testdata/interp/itpl/fae/sugar.fae --color never
  6
  $ spectec rfae eval -p ../../../testdata/interp/itpl/rfae/sum.rfae --color never
  55
  $ spectec bfae eval -p ../../../testdata/interp/itpl/bfae/alias.bfae --color never
  7
  $ spectec mfae eval -p ../../../testdata/interp/itpl/mfae/counter.mfae --color never
  2
  $ spectec lfae eval -p ../../../testdata/interp/itpl/lfae/unused.lfae --color never
  7
  $ spectec kfae eval -p ../../../testdata/interp/itpl/kfae/escape.kfae --color never
  3
  $ spectec tfae typecheck -p ../../../testdata/interp/itpl/tfae/twice.tfae --color never
  num
  $ spectec tfae eval -p ../../../testdata/interp/itpl/tfae/twice.tfae --color never
  42
  $ spectec trfae typecheck -p ../../../testdata/interp/itpl/trfae/sum.trfae --color never
  num
  $ spectec trfae eval -p ../../../testdata/interp/itpl/trfae/sum.trfae --color never
  55

  $ spectec tvfae typecheck -p ../../../testdata/interp/itpl/tvfae/variant.tvfae --color never
  num
  $ spectec tvfae eval -p ../../../testdata/interp/itpl/tvfae/variant.tvfae --color never
  8
