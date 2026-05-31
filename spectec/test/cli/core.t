impty CLI: core operations on the base spec, against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ BASE=../../testdata/interp/impty/base

Typecheck a well-typed program:

  $ spectec impty typecheck --spec $SPEC -p $BASE/hello.imp --color never
  Typecheck succeeded

Evaluate it to a final environment:

  $ spectec impty eval --spec $SPEC -p $BASE/hello.imp --color never
  [
    y -> true,
    x -> 5
  ]

The SL interpreter (--sl) produces the same environment:

  $ spectec impty eval --spec $SPEC -p $BASE/hello.imp --color never --sl
  [
    y -> true,
    x -> 5
  ]

Parse it to an IL value:

  $ spectec impty parse --spec $SPEC -p $BASE/hello.imp --color never
  (((INT) (`ID "x") = (`NUM 5)) ; ((BOOL) (`ID "y") = ((`ID "x") <= (`NUM 10))))

A static type error renders a diagnostic to stderr and exits nonzero:

  $ spectec impty typecheck --spec $SPEC -p $BASE/_errors_undeclared.imp --color never
  error: invocation of relation Check_prog failed
    --> ../../specs/impty/base/spec.spectec:141:6
      |
  141 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../specs/impty/base/spec.spectec:141:6-141:19:
      |     invocation of relation Check_command failed
      |     ├── ../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/skip failed
      |     │   └── ../../specs/impty/base/spec.spectec:105:11-105:15:
      |     │       condition command matches `SKIP` was not met
      |     ├── ../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/decl failed
      |     │   └── ../../specs/impty/base/spec.spectec:108:12-108:23:
      |     │       condition command matches `% % = %` was not met
      |     ├── ../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/assign failed
      |     │   └── ../../specs/impty/base/spec.spectec:114:9-114:43:
      |     │       condition ($lookup_<id, type>(tenv, x) = ?(type)) was not met
      |     ├── ../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/ite failed
      |     │   └── ../../specs/impty/base/spec.spectec:117:12-117:38:
      |     │       condition command matches `IF % THEN % ELSE % END` was not met
      |     ├── ../../specs/impty/base/spec.spectec:141:6-141:19:
      |     │   application of rule Check_command/while failed
      |     │   └── ../../specs/impty/base/spec.spectec:123:12-123:28:
      |     │       condition command matches `WHILE % DO % END` was not met
      |     └── ../../specs/impty/base/spec.spectec:141:6-141:19:
      |         application of rule Check_command/seq failed
      |         └── ../../specs/impty/base/spec.spectec:128:12-128:22:
      |             condition command matches `% ; %` was not met
  [1]
