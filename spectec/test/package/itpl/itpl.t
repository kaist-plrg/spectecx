The ITPL package owns both plugins and their separate default specifications.

  $ grep -q 'target_plugins/ae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'target_plugins/vae/META' ../../../../spectec-target-itpl.install
  $ grep -q 'ae/spec.spectec' ../../../../spectec-target-itpl.install
  $ grep -q 'vae/spec.spectec' ../../../../spectec-target-itpl.install

  $ spectec --help | grep -E '^  (ae|vae) '
    ae                         . AE commands
    vae                        . VAE commands

Both commands use the installed specifications without a --spec override.

  $ spectec ae eval -p ../../../testdata/interp/itpl/ae/arithmetic.ae --color never
  4
  $ spectec vae eval -p ../../../testdata/interp/itpl/vae/shadowing.vae --color never
  21
