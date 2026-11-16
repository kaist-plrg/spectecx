impty CLI: trace rules against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

The rules level adds each rule attempt's enter/exit, showing which rules were
tried and failed before the one that fired:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --trace.level rules
  [ 0] → Run_prog
  [ 1]   → Run_prog/
  [ 1]   → Check_prog
  [ 2]     → Check_prog/
  [ 2]     → Check_command
  [ 3]       → Check_command/skip
  [ 3]       ← Check_command/skip [fail]
  [ 3]       → Check_command/decl
  [ 3]       ← Check_command/decl [fail]
  [ 3]       → Check_command/assign
  [ 3]       ← Check_command/assign [fail]
  [ 3]       → Check_command/ite
  [ 3]       ← Check_command/ite [fail]
  [ 3]       → Check_command/while
  [ 3]       ← Check_command/while [fail]
  [ 3]       → Check_command/seq
  [ 3]       → Check_command
  [ 4]         → Check_command/skip
  [ 4]         ← Check_command/skip [fail]
  [ 4]         → Check_command/decl
  [ 4]         → Check_expr
  [ 5]           → Check_expr/num
  [ 5]           ← Check_expr/num [ok]
  [ 4]         ← Check_expr [ok]
  [ 4]         ← Check_command/decl [ok]
  [ 3]       ← Check_command [ok]
  [ 3]       → Check_command
  [ 4]         → Check_command/skip
  [ 4]         ← Check_command/skip [fail]
  [ 4]         → Check_command/decl
  [ 4]         → Check_expr
  [ 5]           → Check_expr/num
  [ 5]           ← Check_expr/num [fail]
  [ 5]           → Check_expr/boollit
  [ 5]           ← Check_expr/boollit [fail]
  [ 5]           → Check_expr/id
  [ 5]           ← Check_expr/id [fail]
  [ 5]           → Check_expr/not
  [ 5]           ← Check_expr/not [fail]
  [ 5]           → Check_expr/add
  [ 5]           ← Check_expr/add [fail]
  [ 5]           → Check_expr/leq
  [ 5]           → Check_expr
  [ 6]             → Check_expr/num
  [ 6]             ← Check_expr/num [fail]
  [ 6]             → Check_expr/boollit
  [ 6]             ← Check_expr/boollit [fail]
  [ 6]             → Check_expr/id
  [ 6]             → $lookup
  [ 6]             ← $lookup
  [ 6]             ← Check_expr/id [ok]
  [ 5]           ← Check_expr [ok]
  [ 5]           → Check_expr
  [ 6]             → Check_expr/num
  [ 6]             ← Check_expr/num [ok]
  [ 5]           ← Check_expr [ok]
  [ 5]           ← Check_expr/leq [ok]
  [ 4]         ← Check_expr [ok]
  [ 4]         ← Check_command/decl [ok]
  [ 3]       ← Check_command [ok]
  [ 3]       ← Check_command/seq [ok]
  [ 2]     ← Check_command [ok]
  [ 2]     ← Check_prog/ [ok]
  [ 1]   ← Check_prog [ok]
  [ 1]   → Eval_prog
  [ 2]     → Eval_prog/
  [ 2]     → Eval_command
  [ 3]       → Eval_command/skip
  [ 3]       ← Eval_command/skip [fail]
  [ 3]       → Eval_command/decl
  [ 3]       ← Eval_command/decl [fail]
  [ 3]       → Eval_command/assign
  [ 3]       ← Eval_command/assign [fail]
  [ 3]       → Eval_command/ite-true
  [ 3]       ← Eval_command/ite-true [fail]
  [ 3]       → Eval_command/ite-false
  [ 3]       ← Eval_command/ite-false [fail]
  [ 3]       → Eval_command/while-false
  [ 3]       ← Eval_command/while-false [fail]
  [ 3]       → Eval_command/while-true
  [ 3]       ← Eval_command/while-true [fail]
  [ 3]       → Eval_command/seq
  [ 3]       → Eval_command
  [ 4]         → Eval_command/skip
  [ 4]         ← Eval_command/skip [fail]
  [ 4]         → Eval_command/decl
  [ 4]         → Eval_expr
  [ 5]           → Eval_expr/num
  [ 5]           ← Eval_expr/num [ok]
  [ 4]         ← Eval_expr [ok]
  [ 4]         ← Eval_command/decl [ok]
  [ 3]       ← Eval_command [ok]
  [ 3]       → Eval_command
  [ 4]         → Eval_command/skip
  [ 4]         ← Eval_command/skip [fail]
  [ 4]         → Eval_command/decl
  [ 4]         → Eval_expr
  [ 5]           → Eval_expr/num
  [ 5]           ← Eval_expr/num [fail]
  [ 5]           → Eval_expr/boollit
  [ 5]           ← Eval_expr/boollit [fail]
  [ 5]           → Eval_expr/id
  [ 5]           ← Eval_expr/id [fail]
  [ 5]           → Eval_expr/not
  [ 5]           ← Eval_expr/not [fail]
  [ 5]           → Eval_expr/add
  [ 5]           ← Eval_expr/add [fail]
  [ 5]           → Eval_expr/leq
  [ 5]           → Eval_expr
  [ 6]             → Eval_expr/num
  [ 6]             ← Eval_expr/num [fail]
  [ 6]             → Eval_expr/boollit
  [ 6]             ← Eval_expr/boollit [fail]
  [ 6]             → Eval_expr/id
  [ 6]             → $lookup
  [ 6]             ← $lookup
  [ 6]             ← Eval_expr/id [ok]
  [ 5]           ← Eval_expr [ok]
  [ 5]           → Eval_expr
  [ 6]             → Eval_expr/num
  [ 6]             ← Eval_expr/num [ok]
  [ 5]           ← Eval_expr [ok]
  [ 5]           ← Eval_expr/leq [ok]
  [ 4]         ← Eval_expr [ok]
  [ 4]         ← Eval_command/decl [ok]
  [ 3]       ← Eval_command [ok]
  [ 3]       ← Eval_command/seq [ok]
  [ 2]     ← Eval_command [ok]
  [ 2]     ← Eval_prog/ [ok]
  [ 1]   ← Eval_prog [ok]
  [ 1]   ← Run_prog/ [ok]
  [ 0] ← Run_prog [ok]
  [
    y -> true,
    x -> 5
  ]

When an error ends the run, the trace prints a failed exit line for every open relation and rule.

In AL mode a relation can fail to match without raising. This test uses overlapping rules and `--det` to raise an error.

  $ OVERLAY=determinism-overlay.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp
  $ spectec impty typecheck --spec $SPEC --spec $OVERLAY -p $HELLO --color never --det --trace.level rules 2>/dev/null | tail -n 6
  [ 3]       ← Check_command/decl-dup [ok]
  [ 2]     ← Check_command [fail]
  [ 2]     ← Check_command/seq [fail]
  [ 1]   ← Check_command [fail]
  [ 1]   ← Check_prog/ [fail]
  [ 0] ← Check_prog [fail]

  $ ERR=../../testdata/interp/impty/base/_errors_add_bool.imp
  $ spectec impty typecheck --spec $SPEC -p $ERR --color never --sl --trace.level rules
  [ 0] → Check_prog
  [ 1]   → Check_prog/0
  [ 1]   → Check_command
  [ 2]     → Check_command/0
  [ 2]     → Check_expr
  [ 3]       → Check_expr/0
  [ 3]       → Check_expr
  [ 4]         → Check_expr/0
  [ 4]         ← Check_expr/0 [ok]
  [ 3]       ← Check_expr [ok]
  [ 3]       ← Check_expr/0 [fail]
  [ 2]     ← Check_expr [fail]
  [ 2]     ← Check_command/0 [fail]
  [ 1]   ← Check_command [fail]
  [ 1]   ← Check_prog/0 [fail]
  [ 0] ← Check_prog [fail]
  error: relation was not matched
    --> ../../specs/impty/base/spec.spectec:110:6
      |
  110 |   -- Check_expr: tenv |- e : t
      |      ^^^^^^^^^^
      |
      | source: sl-interp
  [1]
