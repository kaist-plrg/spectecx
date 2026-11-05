The P4 slice x[l +: w] selects w bits starting at bit l, so x[2 +: 4] has type
bit<4>.

  $ cat > slice.p4 <<EOF
  > const bit<8> x = 8w0xab;
  > const bit<4> y = x[2 +: 4];
  > EOF

  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p slice.p4 --color never 2>/dev/null
  Typechecker succeeded

The typechecker rejects a constant declaration whose type width differs from
the slice width. The relation ConstDecl_ok reports the failure:

  $ cat > slice-width.p4 <<EOF
  > const bit<8> x = 8w0xab;
  > const bit<3> y = x[2 +: 4];
  > EOF

  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p slice-width.p4 --color never 2>&1 | grep -o "relation ConstDecl_ok failed"
  relation ConstDecl_ok failed

A slice written with +: can also be assigned to:

  $ cat > slice-assign.p4 <<EOF
  > control c() {
  >   apply {
  >     bit<8> x = 8w0;
  >     x[2 +: 4] = 4w3;
  >   }
  > }
  > EOF

  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p slice-assign.p4 --color never 2>/dev/null
  Typechecker succeeded

As in p4c, + and : are separate tokens, so an annotation body may contain +:
next to other tokens:

  $ cat > slice-annotation.p4 <<EOF
  > @foo(a+:b)
  > const bit<8> x = 8w0;
  > EOF

  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p slice-annotation.p4 --color never 2>/dev/null
  Typechecker succeeded
