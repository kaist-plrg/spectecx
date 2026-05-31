impty CLI: tree inputs against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

The inputs level additionally annotates each node with its inputs and outputs:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --tree.level inputs
  Run_prog
  │   in: int x = 5 ; bool y = x <= 10
  │   out: [ y -> true, x -> 5 ]
  ├── Check_prog
  │   │   in: int x = 5 ; bool y = x <= 10
  │   └── Check_command/seq
  │       │   in: []
  │       │   in: int x = 5 ; bool y = x <= 10
  │       │   out: [ y -> bool, x -> int ]
  │       ├── Check_command/decl
  │       │   │   in: []
  │       │   │   in: int x = 5
  │       │   │   out: [ x -> int ]
  │       │   └── Check_expr/num
  │       │           in: []
  │       │           in: 5
  │       │           out: int
  │       └── Check_command/decl
  │           │   in: [ x -> int ]
  │           │   in: bool y = x <= 10
  │           │   out: [ y -> bool, x -> int ]
  │           └── Check_expr/leq
  │               │   in: [ x -> int ]
  │               │   in: x <= 10
  │               │   out: bool
  │               ├── Check_expr/id
  │               │   │   in: [ x -> int ]
  │               │   │   in: x
  │               │   │   out: int
  │               │   └── $lookup_
  │               │           in: [ x -> int ]
  │               │           in: x
  │               │           out: Some(int)
  │               └── Check_expr/num
  │                       in: [ x -> int ]
  │                       in: 10
  │                       out: int
  └── Eval_prog
      │   in: int x = 5 ; bool y = x <= 10
      │   out: [ y -> true, x -> 5 ]
      └── Eval_command/seq
          │   in: []
          │   in: int x = 5 ; bool y = x <= 10
          │   out: [ y -> true, x -> 5 ]
          ├── Eval_command/decl
          │   │   in: []
          │   │   in: int x = 5
          │   │   out: [ x -> 5 ]
          │   └── Eval_expr/num
          │           in: []
          │           in: 5
          │           out: 5
          └── Eval_command/decl
              │   in: [ x -> 5 ]
              │   in: bool y = x <= 10
              │   out: [ y -> true, x -> 5 ]
              └── Eval_expr/leq
                  │   in: [ x -> 5 ]
                  │   in: x <= 10
                  │   out: true
                  ├── Eval_expr/id
                  │   │   in: [ x -> 5 ]
                  │   │   in: x
                  │   │   out: 5
                  │   └── $lookup_
                  │           in: [ x -> 5 ]
                  │           in: x
                  │           out: Some(5)
                  └── Eval_expr/num
                          in: [ x -> 5 ]
                          in: 10
                          out: 10
  [
    y -> true,
    x -> 5
  ]
