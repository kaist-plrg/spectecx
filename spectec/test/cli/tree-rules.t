impty CLI: tree rules against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

The rules level renders the derivation as an ASCII tree of relation and
function calls:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --tree.level rules
  Run_prog
  -- Check_prog
     -- Check_command/seq
        -- Check_command/decl
           -- Check_expr/num
        -- Check_command/decl
           -- Check_expr/leq
              -- Check_expr/id
                 -- $lookup_
              -- Check_expr/num
  -- Eval_prog
     -- Eval_command/seq
        -- Eval_command/decl
           -- Eval_expr/num
        -- Eval_command/decl
           -- Eval_expr/leq
              -- Eval_expr/id
                 -- $lookup_
              -- Eval_expr/num
  [
    y -> true,
    x -> 5
  ]
