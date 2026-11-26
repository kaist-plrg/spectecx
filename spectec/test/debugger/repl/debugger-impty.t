Debugger on impty: breakpoints and stepping across nested relations.

  $ SPEC=../../../specs/impty/base/spec.spectec
  $ PROG=../../../testdata/interp/impty/base/hello.imp
  $ debug() { spectec impty debug eval --spec $SPEC -p $PROG --color never; }

A relation breakpoint stops at the call, and show prints the relation:

  $ printf 'b Check_prog\nr\nbt\nshow\nc\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b Check_prog
  Breakpoint 1 at Check_prog.
  (dbg) r
  Breakpoint 1, Check_prog (called at ../../../specs/impty/base/spec.spectec:256:6-256:16)
  (dbg) bt
  #0  Check_prog (called at ../../../specs/impty/base/spec.spectec:256:6-256:16)
  #1  Run_prog
  (dbg) show
  ../../../specs/impty/base/spec.spectec:135:1-138:48
      133  ;;--- Program ---
      134  
   >  135  relation Check_prog:
   >  136    |- prog
   >  137    hint(input %0)
   >  138    hint(prose_true "program" %0 "is well-typed")
      139  
      140  rule Check_prog:
  (dbg) c
  [
    y -> true,
    x -> 5
  ]

The decl rule has one written premise, so next leaves for the caller's next
premise; step enters a call, and finish returns from one call at a time:

  $ printf 'b Check_command/decl\nr\nn\nn\nlocals\ns\ns\nbt\nf\nf\nd 1\nc\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b Check_command/decl
  Breakpoint 1 at Check_command/decl.
  (dbg) r
  Breakpoint 1, Check_command/decl (called at ../../../specs/impty/base/spec.spectec:130:6-130:19)
      110    -- Check_expr: tenv |- e : t
  (dbg) n
  Check_command/seq, ../../../specs/impty/base/spec.spectec:131
      131    -- Check_command: tenv_1 |- c_2 -| tenv_2
  (dbg) n
  Breakpoint 1, Check_command/decl (called at ../../../specs/impty/base/spec.spectec:131:6-131:19)
      110    -- Check_expr: tenv |- e : t
  (dbg) locals
    command = bool y = (x <= 10)
    e = x <= 10
    t = bool
    tenv = [
             x -> int
           ]
    x = y
  (dbg) s
  Check_expr/leq, ../../../specs/impty/base/spec.spectec:89
       89    -- Check_expr: tenv |- e_l : INT
  (dbg) s
  Check_expr/id, ../../../specs/impty/base/spec.spectec:76
       76    -- if $lookup<id, type>(tenv, x) = t
  (dbg) bt
  #0  Check_expr/id (called at ../../../specs/impty/base/spec.spectec:89:6-89:16)
  #1  Check_expr/leq (called at ../../../specs/impty/base/spec.spectec:110:6-110:16)
  #2  Check_command/decl (called at ../../../specs/impty/base/spec.spectec:131:6-131:19)
  #3  Check_command/seq (called at ../../../specs/impty/base/spec.spectec:142:6-142:19)
  #4  Check_prog (called at ../../../specs/impty/base/spec.spectec:256:6-256:16)
  #5  Run_prog
  (dbg) f
  Check_expr returned:
    0: int
  (dbg) f
  Check_expr returned:
    0: bool
  (dbg) d 1
  (dbg) c
  [
    y -> true,
    x -> 5
  ]

print takes a local by the name locals shows, or by its identifier alone:

  $ printf 'b spec.spectec:11\nr\nlocals\np K_t*\np K_t\np pair<K, V>*\np pair\nq\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b spec.spectec:11
  Breakpoint 1 at ../../../specs/impty/base/spec.spectec:11 in $lookup.
  (dbg) r
  Breakpoint 1, $lookup (called at ../../../specs/impty/base/spec.spectec:76:10-76:17)
       11    -- if K_h = K_query
  (dbg) locals
    K_h = x
    K_query = x
    K_t* = []
    V_h = int
    V_t* = []
    pair<K, V>* = [
                    x -> int
                  ]
  (dbg) p K_t*
    K_t* = []
  (dbg) p K_t
    K_t* = []
  (dbg) p pair<K, V>*
    pair<K, V>* = [
                    x -> int
                  ]
  (dbg) p pair
  No local `pair`.
  (dbg) q
  [1]

Stepping stops where a premise fails, before the run tries the next rule.
while-false's premise fails because e evaluates to true:

  $ printf 'b Eval_command/while-false\nr\nn\np literal\nn\nq\n' | spectec impty debug eval --spec $SPEC -p ../../../testdata/interp/impty/base/loop.imp --color never
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b Eval_command/while-false
  Breakpoint 1 at Eval_command/while-false.
  (dbg) r
  Breakpoint 1, Eval_command/while-false (called at ../../../specs/impty/base/spec.spectec:235:6-235:18)
      224    -- Eval_expr: env |- e ==> (_BOOL false)
  (dbg) n
  Eval_command/while-false failed at ../../../specs/impty/base/spec.spectec:224
      224    -- Eval_expr: env |- e ==> (_BOOL false)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ failed
  (dbg) p literal
    literal = true
  (dbg) n
  Eval_command/while-true, ../../../specs/impty/base/spec.spectec:228
      228    -- Eval_expr: env |- e ==> (_BOOL true)
  (dbg) q
  [1]

Quitting aborts the run:

  $ printf 'q\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) q
  [1]
