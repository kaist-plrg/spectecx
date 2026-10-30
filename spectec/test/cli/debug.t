Debug output separates the location from the expression with one colon in each interpreter.

  $ awk '{ print } /^  tenv \|- SKIP -\| tenv$/ { print "  -- debug 0" }' ../../specs/impty/base/spec.spectec > debug.spectec
  $ for mode in '' --sl --pl; do spectec impty typecheck --spec debug.spectec -p ../../testdata/interp/impty/base/skip.imp --color never $mode; done
  debug.spectec:107:12-107:13: 0
  0
  Typecheck succeeded
  debug.spectec:107:12-107:13: 0
  0
  Typecheck succeeded
  debug.spectec:107:12-107:13: 0
  0
  Typecheck succeeded
