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

Parse and reprint it in surface syntax:

  $ spectec impty parse --spec $SPEC -p $BASE/hello.imp --color never
  int x = 5;
  bool y = x <= 10

A static type error renders a diagnostic to stderr and exits nonzero:

  $ spectec impty typecheck --spec $SPEC -p $BASE/_errors_undeclared.imp --color never
  error: invocation of relation Check_prog failed
    --> ../../specs/impty/base/spec.spectec:142:6
      |
  142 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../specs/impty/base/spec.spectec:142:6-142:19:
      |     invocation of relation Check_command failed
      |     └── ../../specs/impty/base/spec.spectec:142:6-142:19:
      |         application of rule Check_command/assign failed
      |         └── ../../specs/impty/base/spec.spectec:115:9-115:39:
      |             condition ($lookup<id, type>(tenv, x) = ?(t)) was not met
  [1]

The command-line parser rejects --sl together with --pl:

  $ spectec impty eval --spec $SPEC -p $BASE/hello.imp --color never --sl --pl > flags.out 2>&1
  [1]
  $ sed '/^$/d;s/ *$//' flags.out
  Error parsing command line:
    Cannot pass more than one of these:
      --sl
      --pl
  For usage information, run
    spectec impty eval -help

The command-line parser also rejects --spec together with --spec-dir:

  $ spectec impty eval --spec $SPEC --spec-dir ../../specs/impty/base -p $BASE/hello.imp --color never > flags.out 2>&1
  [1]
  $ sed '/^$/d;s/ *$//' flags.out
  Error parsing command line:
    Cannot pass more than one of these:
      --spec-dir
      --spec
  For usage information, run
    spectec impty eval -help

The cram runner removes ANSI escape codes from captured output. These cases use cat -v to make escape codes visible before the runner captures them.

spectec styles diagnostics with --color always, even when stderr is not a terminal:

  $ spectec impty typecheck --spec $SPEC -p $BASE/_errors_undeclared.imp --color always 2>&1 | head -n 1 | cat -v
  ^[[1m^[[31merror: ^[[0m^[[1minvocation of relation Check_prog failed^[[0m

spectec leaves diagnostics plain with --color auto when stderr is not a terminal:

  $ spectec impty typecheck --spec $SPEC -p $BASE/_errors_undeclared.imp --color auto 2>&1 | head -n 1 | cat -v
  error: invocation of relation Check_prog failed

spectec leaves the command-line parser's wording for rejected --color values unspecified.

The command-line parser rejects the unknown --color value bogus:

  $ spectec impty eval --spec $SPEC -p $BASE/hello.imp --color bogus > flags.out 2>&1
  [1]
  $ head -n 1 flags.out
  Error parsing command line:
