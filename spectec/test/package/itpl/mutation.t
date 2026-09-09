BFAE and MFAE evaluations agree across IL, SL, and PL.

  $ check() {
  >   target=$1
  >   expected=$2
  >   expression=$3
  >   printf '%s\n' "$expression" > "input.$target"
  >   for mode in '' --sl --pl; do
  >     actual=$(spectec "$target" eval $mode -p "input.$target" --color never) || return 1
  >     if [ "$actual" != "$expected" ]; then
  >       printf '%s %s: expected %s, got %s\n' "$target" "$mode" "$expected" "$actual"
  >       return 1
  >     fi
  >   done
  > }

BFAE allocates distinct boxes after evaluating their initializers. Opening and updating boxes uses the store produced by the preceding computation.

  $ check bfae 1 '!(box 1)'
  $ check bfae -3 '!box -3'
  $ check bfae 7 'val outer = box (box 1) in val inner = !outer in inner := 7; !!outer'
  $ check bfae 3 'val b = box 1 in val c = box 2 in b := 3; !c + !b - 2'
  $ check bfae 2 'val b = box 1 in !(b := 2; b)'
  $ check bfae 7 'val b = box 0 in b := box 7; !!b'
  $ check bfae 7 '!((box 0) := box 7)'
  $ check bfae 5 'val b = box 0 in b := 5'
  $ check bfae 1 'box 0'
  $ check bfae 1 'val b = box 0 in b := b; !!b'

BFAE shares box addresses through aliases and closures, including functions stored inside boxes.

  $ check bfae 2 'val b = box 1 in val alias = b in alias := 2; !b'
  $ check bfae 2 'val b = box 1 in (fun x -> x := 2) b; !b'
  $ check bfae 5 'val b = box 1 in val f = box (fun x -> !b + x) in b := 3; (!f) 2'
  $ check bfae 7 'val b = box 0 in b := (fun x -> x + 1); (!b) 6'
  $ check bfae 3 'val make = fun x -> val b = box x in fun ignored -> b := !b + 1 in val f = make 1 in f 0; f 0'

BFAE passes stores from left to right through arithmetic, function application, and both sides of assignment.

  $ check bfae 4 'val b = box 0 in (b := 2) + !b'
  $ check bfae 0 'val b = box 0 in (b := 2) - !b'
  $ check bfae 6 'val b = box 1 in (b := 2; fun x -> !b + x) (b := 3)'
  $ check bfae 4 'val b = box 0 in val c = box 0 in (b := 1; b) := (c := 2); !b + !c'
  $ check bfae 7 'val b = box 0 in val c = box 0 in (b := 1; c) := !b + 2; !b + !c + 3'
  $ check bfae 8 'val b = box 0 in val c = box 0 in b := c := 4; !b + !c'

MFAE assigns values to variables and gives each function parameter a fresh address. Passing a variable copies its value, so assigning to the parameter does not assign to the caller's variable.

  $ check mfae 5 'val x = 1 in x := 5'
  $ check mfae 1 'val x = 1 in (fun y -> y := 2) x; x'
  $ check mfae 1 'val x = 1 in (fun y -> x := 2; y) x'
  $ check mfae 7 '(fun f -> f 0) (val y = 7 in fun ignored -> y)'
  $ check mfae 7 'val x = 0 in x := (val y = 7 in fun ignored -> y); x 0'
  $ check mfae 3 'val x = 0 in x := (x := 2) + 1; x'
  $ check mfae 8 'val x = 0 in val y = 0 in x := y := 4; x + y'

MFAE closures share captured variable addresses and retain independent addresses for separate calls and shadowed bindings.

  $ check mfae 2 'val x = 1 in val f = fun ignored -> x in x := 2; f 0'
  $ check mfae 102 'val x = 1 in val f = fun ignored -> x := x + 1 in val x = 100 in f 0 + x'
  $ check mfae 13 'val make = fun x -> fun ignored -> x := x + 1 in val first = make 0 in val second = make 10 in first 0; first 0 + second 0'
  $ check mfae 3 'val make = fun x -> fun ignored -> x := x + 1 in val f = make 1 in val alias = f in f 0; alias 0'
  $ check mfae 9 'val f = fun x -> x + 1 in val g = fun x -> f x in f := (fun x -> x + 2); g 7'
  $ check mfae 4 'val x = 1 in (x := 2) + x'
  $ check mfae 0 'val x = 1 in (x := 2) - x'
  $ check mfae 6 'val x = 1 in (x := 2; fun y -> x + y) (x := 3)'

MFAE sequencing introduces an unused parameter without capturing names in expressions, closures, or assignment targets.

  $ check mfae 30 "val __seq = 10 in val __seq' = 20 in 0; __seq + __seq'"
  $ check mfae 2 'val __seq = 1 in (0; __seq := 2); __seq'
  $ check mfae 7 '(val __seq = 4 in 0; fun x -> __seq + x) 3'
  $ check mfae 3 '1; 2; 3'

Both targets retain nonrecursive binding sugar, function scope, signed arithmetic, and higher-order functions.

  $ for target in bfae mfae; do
  >   check "$target" -3 '(fun x -> x) (-3)' || exit 1
  >   check "$target" 21 'val x = 10 in (val x = x + 1 in x) + x' || exit 1
  >   check "$target" 42 'val twice = fun f -> fun x -> f (f x) in twice (fun n -> n + 1) 40' || exit 1
  >   check "$target" 6 'def twice(x) = x + x in twice 3' || exit 1
  >   check "$target" 5 'val f = fun x -> x + 1 in def f(x) = f x in f 4' || exit 1
  > done

Expected failures must come from evaluation after parsing succeeds.

  $ rejects() {
  >   target=$1
  >   expression=$2
  >   printf '%s\n' "$expression" > "input.$target"
  >   spectec "$target" parse -p "input.$target" --color never > /dev/null || return 1
  >   for mode in '' --sl --pl; do
  >     if spectec "$target" eval $mode -p "input.$target" --color never > error 2>&1; then
  >       printf 'unexpected success: %s\n' "$expression"
  >       return 1
  >     fi
  >     grep -Eq 'source: (il|sl|pl)-interp' error || { cat error; return 1; }
  >   done
  > }

Boxes are required for reads and writes. Box addresses cannot be used as integers or functions.

  $ rejects bfae '!1'
  $ rejects bfae '!(fun x -> x)'
  $ rejects bfae '1 := 2'
  $ rejects bfae '(fun x -> x) := 2'
  $ rejects bfae '(box 1) + 2'
  $ rejects bfae '2 - (box 1)'
  $ rejects bfae '(box 1) 2'
  $ rejects bfae 'box missing'
  $ rejects bfae 'val b = box 0 in b := missing'

MFAE requires an existing variable for assignment. Sequencing cannot bind a free identifier or expose a local variable outside its scope.

  $ rejects mfae 'missing := 1'
  $ rejects mfae 'val x = 1 in x := missing'
  $ rejects mfae '0; __seq'
  $ rejects mfae '((fun x -> x := 1) 0); x'

Both targets reject free variables and invalid values, and evaluate discarded expressions and arguments eagerly.

  $ for target in bfae mfae; do
  >   rejects "$target" 'missing; 1' || exit 1
  >   rejects "$target" '(fun x -> 0) missing' || exit 1
  >   rejects "$target" 'def f(x) = f x in f 1' || exit 1
  >   rejects "$target" 'val f = fun x -> x + y in val y = 1 in f 2' || exit 1
  >   rejects "$target" '1 2' || exit 1
  >   rejects "$target" '(fun x -> x) + 1' || exit 1
  > done
