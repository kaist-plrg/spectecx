P4 CLI: trace full against header declarations.

The P4 spec supplies examples of iterations and builtin calls for these trace tests.

  $ cat > header.p4 <<EOF
  > header h_t { bit<8> a; bit<8> b; }
  > EOF
  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p header.p4 --color never --trace.level full > al.txt 2>/dev/null
  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p header.p4 --color never --sl --trace.level full > sl.txt 2>/dev/null
  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p header.p4 --color never --pl --trace.level full > pl.txt 2>/dev/null

The header has two fields, so each iteration runs twice. The trace shows the second repetition of the first iteration and both repetitions of the following iteration. Each pair of repetition lines shows the position, total and result. The trace indents the instructions inside each repetition by one step and marks the iterated instruction with `*` in SL and PL mode.

  $ awk '/→ repetition 2 of 2/ { p = 1 } p { print } /← repetition 2 of 2/ && ++n == 2 { exit }' al.txt
  [ 3]       → repetition 2 of 2
  [ 4]         | -- rel Type_ok: block TC_1 |- type_f as typeOrVoid : typeIR_f # typeId'*{typeId' <- typeId'*}
  [ 4]         → Type_ok
  [ 4]             block
  [ 4]             { control cont; loop out; global { constructor { [] }; type { [] }; callable { [] }; frame { [] }...
  [ 4]             bit < 8 >
  [ 4]         ← Type_ok [ok]
  [ 4]             bit < 8 >
  [ 4]             []
  [ 3]       ← repetition 2 of 2 [ok]
  [ 3]       → repetition 1 of 2
  [ 4]         | -- if typeId'*{typeId' <- typeId'*} matches []
  [ 3]       ← repetition 1 of 2 [ok]
  [ 3]       → repetition 2 of 2
  [ 4]         | -- if typeId'*{typeId' <- typeId'*} matches []
  [ 3]       ← repetition 2 of 2 [ok]
  $ awk '/→ repetition 2 of 2/ { p = 1 } p { print } /← repetition 2 of 2/ && ++n == 2 { exit }' sl.txt
  [ 3]       → repetition 2 of 2
  [ 4]         | (Type_ok: (block) TC_1 |- (type_f as typeOrVoid) : typeIR_f # typeId'*)*
  [ 4]         → Type_ok
  [ 4]             block
  [ 4]             { control cont; loop out; global { constructor { [] }; type { [] }; callable { [] }; frame { [] }...
  [ 4]             bit < 8 >
  [ 4]         ← Type_ok [ok]
  [ 4]             bit < 8 >
  [ 4]             []
  [ 3]       ← repetition 2 of 2 [ok]
  [ 3]       → repetition 1 of 2
  [ 4]         | If ((typeId'* matches pattern []))*, then
  [ 3]       ← repetition 1 of 2 [ok]
  [ 3]       → repetition 2 of 2
  [ 4]         | If ((typeId'* matches pattern []))*, then
  [ 3]       ← repetition 2 of 2 [ok]
  $ awk '/→ repetition 2 of 2/ { p = 1 } p { print } /← repetition 2 of 2/ && ++n == 2 { exit }' pl.txt
  [ 3]       → repetition 2 of 2
  [ 4]         | (Type_ok: block TC_1 |- (type_f as typeOrVoid) : typeIR_f # (typeId')*)*
  [ 4]         → Type_ok
  [ 4]             block
  [ 4]             { control cont; loop out; global { constructor { [] }; type { [] }; callable { [] }; frame { [] }...
  [ 4]             bit < 8 >
  [ 4]         ← Type_ok [ok]
  [ 4]             bit < 8 >
  [ 4]             []
  [ 3]       ← repetition 2 of 2 [ok]
  [ 3]       → repetition 1 of 2
  [ 4]         | If (((typeId')* matches pattern []))*, then
  [ 3]       ← repetition 1 of 2 [ok]
  [ 3]       → repetition 2 of 2
  [ 4]         | If (((typeId')* matches pattern []))*, then
  [ 3]       ← repetition 2 of 2 [ok]

The trace prints arguments and the result for a builtin call. Builtins have no clauses to print.

  $ grep -A5 '→ \$add_map' al.txt
  [ 4]         → $add_map
  [ 4]             { [] }
  [ 4]             h_t
  [ 4]             header h_t < [] > { [ bit < 8 > a ;, bit < 8 > b ; ] }
  [ 4]         ← $add_map
  [ 4]             { [ h_t : header h_t < [] > { [ bit < 8 > a ;, bit < 8 > b ; ] } ] }
  $ grep -A5 '→ \$add_map' sl.txt
  [ 4]         → $add_map
  [ 4]             { [] }
  [ 4]             h_t
  [ 4]             header h_t < [] > { [ bit < 8 > a ;, bit < 8 > b ; ] }
  [ 4]         ← $add_map
  [ 4]             { [ h_t : header h_t < [] > { [ bit < 8 > a ;, bit < 8 > b ; ] } ] }
  $ grep -A5 '→ \$add_map' pl.txt
  [ 4]         → $add_map
  [ 4]             { [] }
  [ 4]             h_t
  [ 4]             header h_t < [] > { [ bit < 8 > a ;, bit < 8 > b ; ] }
  [ 4]         ← $add_map
  [ 4]             { [ h_t : header h_t < [] > { [ bit < 8 > a ;, bit < 8 > b ; ] } ] }

The second field has type `int`, which cannot be nested in a header. In all three modes the corresponding repetition ends with `[fail]`.

  $ cat > int-field.p4 <<EOF
  > header h_t { bit<8> a; int b; }
  > EOF
  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p int-field.p4 --color never --trace.level full 2>/dev/null | grep 'repetition .* \[fail\]'
  [ 5]           ← repetition 2 of 2 [fail]
  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p int-field.p4 --color never --sl --trace.level full 2>/dev/null | grep 'repetition .* \[fail\]'
  [ 5]           ← repetition 2 of 2 [fail]
  $ spectec p4 typecheck --spec-dir ../../specs/p4 -p int-field.p4 --color never --pl --trace.level full 2>/dev/null | grep 'repetition .* \[fail\]'
  [ 5]           ← repetition 2 of 2 [fail]
