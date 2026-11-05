With --det, the P4 typechecker requires exactly one clause or rule to apply to
each input. The typechecker calls $compat_lnot on a boolean negation. The last
clause of $compat_lnot matches every type and applies only when no other clause
does.

  $ cat > lnot.p4 <<EOF
  > const bool t = !true;
  > EOF

  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p lnot.p4 --det --color never 2>/dev/null
  Typechecker succeeded

  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p lnot.p4 --det --sl --color never 2>/dev/null
  Typechecker succeeded

  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p lnot.p4 --det --pl --color never 2>/dev/null
  Typechecker succeeded

When both operands of a sum are typedefs, only the first TYPEDEF clause of
$compat_plusminusmult matches the call, because the second requires a left
operand that is not a typedef:

  $ cat > typedef-plus.p4 <<EOF
  > typedef bit<8> B;
  > const B a = 8w1;
  > const B s = a + a;
  > EOF

  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p typedef-plus.p4 --det --color never 2>/dev/null
  Typechecker succeeded
