impty parse on a spec whose only error comes from the algo pass.

The parse command stops after elaboration, so algo errors do not prevent it
from parsing and unparsing a program.

  $ cat ../../specs/impty/base/spec.spectec > spec.spectec
  $ cat >> spec.spectec <<'SPEC'
  > 
  > var pn : nat
  > var pm : nat
  > 
  > relation Probe: nat |- nat
  >   hint(input %0)
  > 
  > rule Probe/free:
  >   pn |- pm
  > SPEC

The algo command rejects the rule because the rule never binds its output pm.

  $ spectec algo spec.spectec --color never 2>&1 | head -1
  error[algo/free-variable-in-output]: expression has free variable(s): { pm : nat }

  $ spectec impty parse --spec spec.spectec -p ../../testdata/interp/impty/base/hello.imp --color never
  int x = 5;
  bool y = x <= 10
