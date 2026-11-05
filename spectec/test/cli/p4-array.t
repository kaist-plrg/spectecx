A P4 array type T[n] can hold elements that are not headers, such as bit<8>.

  $ cat > array.p4 <<EOF
  > control c() {
  >   apply {
  >     bit<8>[4] a;
  >     a[1] = 8w3;
  >   }
  > }
  > EOF

  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p array.p4 --color never 2>/dev/null
  Typechecker succeeded
