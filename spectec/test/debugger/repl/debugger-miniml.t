Debugger on Mini-ML: breakpoints, stepping and inspection, driven by commands
on stdin. The debugger echoes each command when stdin is not a terminal.

  $ SPEC=../../../specs/miniml/spec.spectec
  $ PROG=../../../testdata/interp/miniml/distr_pair.ml
  $ debug() { spectec miniml debug eval --spec $SPEC -p $PROG --color never; }

The debug group has one command per task:

  $ spectec miniml debug --help
  Run miniml tasks under the interactive debugger
  
    spectec miniml debug SUBCOMMAND
  
  === subcommands ===
  
    eval                       . Run eval under the debugger
    help                       . explain a given subcommand (perhaps recursively)
  

The prompt opens before the run. Commands that need a run refuse until `run`,
bad commands are reported, and with no breakpoints the run goes to the end:

  $ printf 'c\nbt\nfoo\nb Nope\nb a/b/c\nr\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) c
  The run hasn't started.
  (dbg) bt
  The run hasn't started.
  (dbg) foo
  Unknown command `foo`. Try `help`.
  (dbg) b Nope
  No definition matches `Nope`.
  (dbg) b a/b/c
  Cannot read `a/b/c` as Rel, Rel/rule, FILE:LINE or LINE.
  (dbg) r
  42

A rule breakpoint stops once the rule applies, not when it is tried: the pair
rule is tried on every call, but stops only on the two pairs:

  $ printf 'b Eval_expr/pair\nr\nargs\nc\nargs\nc\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b Eval_expr/pair
  Breakpoint 1 at Eval_expr/pair.
  (dbg) r
  Breakpoint 1, Eval_expr/pair (called at ../../../specs/miniml/spec.spectec:152:6-152:15)
  (dbg) args
    0: epair (ecst 40) (ecst 2)
  (dbg) c
  Breakpoint 1, Eval_expr/pair (called at ../../../specs/miniml/spec.spectec:154:6-154:15)
  (dbg) args
    0: epair (eapp (efun x (evar x)) (eapp (eop fst) (epair (ecst 40) (ecst 2)))) (eapp (efun x (evar x)) (eapp (eop snd) (epair (ecst 40) (ecst 2))))
  (dbg) c
  42

A rule whose premises are all guards stops when it succeeds:

  $ printf 'b Eval_expr/cst\nr\nargs\nd 1\ninfo b\nc\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b Eval_expr/cst
  Breakpoint 1 at Eval_expr/cst.
  (dbg) r
  Breakpoint 1, Eval_expr/cst (called at ../../../specs/miniml/spec.spectec:138:6-138:15)
  (dbg) args
    0: ecst 40
  (dbg) d 1
  (dbg) info b
  No breakpoints.
  (dbg) c
  42

Stepping: next steps over calls, step enters them, finish reports the result,
and next after the last premise leaves for the caller:

  $ printf 'b Eval_expr/let\nr\nlocals\nshow\nn\nn\np e\np nope\ns\nbt\nf\nshow\nn\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b Eval_expr/let
  Breakpoint 1 at Eval_expr/let.
  (dbg) r
  Breakpoint 1, Eval_expr/let
  (dbg) locals
    e1 = efun f (efun p (epair (eapp (evar f) (eapp (eop fst) (evar p))) (eapp (evar f) (eapp (eop snd) (evar p)))))
    e2 = eapp (eop add) (eapp (eapp (evar distr_pair) (efun x (evar x))) (epair (ecst 40) (ecst 2)))
    expr = elet distr_pair (efun f (efun p (epair (eapp (evar f) (eapp (eop fst) (evar p))) (eapp (evar f) (eapp (eop snd) (evar p)))))) (eapp (eop add) (eapp (eapp (evar distr_pair) (efun x (evar x))) (epair (ecst 40) (ecst 2))))
    x = distr_pair
  (dbg) show
  ../../../specs/miniml/spec.spectec:141:1-145:25
      139    -- Eval_expr: e2 ==> v2
      140  
   >  141  rule Eval_expr/let:
   >  142    Elet x e1 e2 ==> v2
  =>  143    -- Eval_expr: e1 ==> v1
   >  144    -- if $subst(e2, x, v1) = e
   >  145    -- Eval_expr: e ==> v2
      146  
      147  var body: expr
  (dbg) n
  Eval_expr/let, ../../../specs/miniml/spec.spectec:144
      144    -- if $subst(e2, x, v1) = e
  (dbg) n
  Eval_expr/let, ../../../specs/miniml/spec.spectec:145
      145    -- Eval_expr: e ==> v2
  (dbg) p e
    e = eapp (eop add) (eapp (eapp (efun f (efun p (epair (eapp (evar f) (eapp (eop fst) (evar p))) (eapp (evar f) (eapp (eop snd) (evar p)))))) (efun x (evar x))) (epair (ecst 40) (ecst 2)))
  (dbg) p nope
  No local `nope`.
  (dbg) s
  Eval_expr/add, ../../../specs/miniml/spec.spectec:94
       94    -- Eval_expr: e1 ==> Eop ADD
  (dbg) bt
  #0  Eval_expr/add (called at ../../../specs/miniml/spec.spectec:145:6-145:15)
  #1  Eval_expr/let
  (dbg) f
  Eval_expr returned:
    0: ecst 42
  (dbg) show
  ../../../specs/miniml/spec.spectec:141:1-145:25
      139    -- Eval_expr: e2 ==> v2
      140  
   >  141  rule Eval_expr/let:
   >  142    Elet x e1 e2 ==> v2
   >  143    -- Eval_expr: e1 ==> v1
   >  144    -- if $subst(e2, x, v1) = e
  =>  145    -- Eval_expr: e ==> v2
      146  
      147  var body: expr
  (dbg) n
  42

Premise breakpoints: FILE:LINE stops before the written premise on that line,
naming the file by any suffix of its path. A bare LINE needs a current call,
and sets one in its file:

  $ printf 'b 144\nb spec.spectec:144\nr\nbt\nlocals\nb 145\ninfo b\nc\nc\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b 144
  No call in progress. Use FILE:LINE.
  (dbg) b spec.spectec:144
  Breakpoint 1 at ../../../specs/miniml/spec.spectec:144 in Eval_expr/let.
  (dbg) r
  Breakpoint 1, Eval_expr/let
      144    -- if $subst(e2, x, v1) = e
  (dbg) bt
  #0  Eval_expr/let
  (dbg) locals
    e1 = efun f (efun p (epair (eapp (evar f) (eapp (eop fst) (evar p))) (eapp (evar f) (eapp (eop snd) (evar p)))))
    e2 = eapp (eop add) (eapp (eapp (evar distr_pair) (efun x (evar x))) (epair (ecst 40) (ecst 2)))
    expr = elet distr_pair (efun f (efun p (epair (eapp (evar f) (eapp (eop fst) (evar p))) (eapp (evar f) (eapp (eop snd) (evar p)))))) (eapp (eop add) (eapp (eapp (evar distr_pair) (efun x (evar x))) (epair (ecst 40) (ecst 2))))
    v1 = efun f (efun p (epair (eapp (evar f) (eapp (eop fst) (evar p))) (eapp (evar f) (eapp (eop snd) (evar p)))))
    x = distr_pair
  (dbg) b 145
  Breakpoint 2 at ../../../specs/miniml/spec.spectec:145 in Eval_expr/let.
  (dbg) info b
    1  ../../../specs/miniml/spec.spectec:144 in Eval_expr/let
    2  ../../../specs/miniml/spec.spectec:145 in Eval_expr/let
  (dbg) c
  Breakpoint 2, Eval_expr/let
      145    -- Eval_expr: e ==> v2
  (dbg) c
  42

A rule's head, or a blank line inside it, slides to its next written premise.
Lines with no premise at or after them in a rule or clause are refused:

  $ printf 'b spec.spectec:141\nb spec.spectec:54\nb spec.spectec:99\nb nope.spectec:3\nb spec.spectec:0\nr\nc\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b spec.spectec:141
  Breakpoint 1 at ../../../specs/miniml/spec.spectec:143 in Eval_expr/let.
  (dbg) b spec.spectec:54
  No premise at or after line 54 in $subst.
  (dbg) b spec.spectec:99
  No rule or clause at ../../../specs/miniml/spec.spectec:99.
  (dbg) b nope.spectec:3
  No rule or clause in `nope.spectec`.
  (dbg) b spec.spectec:0
  Cannot read `spec.spectec:0` as Rel, Rel/rule, FILE:LINE or LINE.
  (dbg) r
  Breakpoint 1, Eval_expr/let
      143    -- Eval_expr: e1 ==> v1
  (dbg) c
  42

Function clauses have premises too:

  $ printf 'b spec.spectec:44\nr\nbt\nshow\nd 1\nc\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b spec.spectec:44
  Breakpoint 1 at ../../../specs/miniml/spec.spectec:44 in $subst_var.
  (dbg) r
  Breakpoint 1, $subst_var (called at ../../../specs/miniml/spec.spectec:56:29-56:39)
       44    -- if x = y
  (dbg) bt
  #0  $subst_var (called at ../../../specs/miniml/spec.spectec:56:29-56:39)
  #1  $subst (called at ../../../specs/miniml/spec.spectec:57:38-57:44)
  #2  $subst (called at ../../../specs/miniml/spec.spectec:57:38-57:44)
  #3  $subst (called at ../../../specs/miniml/spec.spectec:57:55-57:61)
  #4  $subst (called at ../../../specs/miniml/spec.spectec:144:10-144:16)
  #5  Eval_expr/let
  (dbg) show
  ../../../specs/miniml/spec.spectec:43:1-44:14
       41    hint(prose_in "(Efun %0 %1)[%2 <- %3]")
       42  
   >   43  def $subst_var(y, x, v) = v
  =>   44    -- if x = y
       45  def $subst_var(y, x, v) = Evar y
       46   -- otherwise
  (dbg) d 1
  (dbg) c
  42

A rule breakpoint and a breakpoint on the rule's first written premise stop
once, together:

  $ printf 'b Eval_expr/let\nb spec.spectec:143\nr\nc\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b Eval_expr/let
  Breakpoint 1 at Eval_expr/let.
  (dbg) b spec.spectec:143
  Breakpoint 2 at ../../../specs/miniml/spec.spectec:143 in Eval_expr/let.
  (dbg) r
  Breakpoints 1 and 2, Eval_expr/let
      143    -- Eval_expr: e1 ==> v1
  (dbg) c
  42

A breakpoint where one already is is not added again, including a line that
moves to a premise that already has one:

  $ printf 'b Eval_expr\nb Eval_expr\nb spec.spectec:141\nb spec.spectec:143\ninfo b\nr\nq\n' | debug
  Set breakpoints, then `run` to start. `help` lists commands.
  (dbg) b Eval_expr
  Breakpoint 1 at Eval_expr.
  (dbg) b Eval_expr
  Breakpoint 1 is already at Eval_expr.
  (dbg) b spec.spectec:141
  Breakpoint 2 at ../../../specs/miniml/spec.spectec:143 in Eval_expr/let.
  (dbg) b spec.spectec:143
  Breakpoint 2 is already at ../../../specs/miniml/spec.spectec:143 in Eval_expr/let.
  (dbg) info b
    1  Eval_expr
    2  ../../../specs/miniml/spec.spectec:143 in Eval_expr/let
  (dbg) r
  Breakpoint 1, Eval_expr
  (dbg) q
  [1]
