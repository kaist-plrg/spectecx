FAE and RFAE evaluations agree across IL, SL, and PL.

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

FAE and RFAE preserve FVAE's behavior when val bindings are translated into function applications.

  $ check fvae 21 'val x = 10 in (val x = x + 1 in x) + x'
  $ for target in fae rfae; do
  >   check "$target" -3 '(2 - 8) + 3' || exit 1
  >   check "$target" -3 '(fun x -> x) (-3)' || exit 1
  >   check "$target" 21 'val x = 10 in (val x = x + 1 in x) + x' || exit 1
  >   check "$target" 11 'val x = 10 in val f = fun y -> x + y in val x = 100 in f 1' || exit 1
  >   check "$target" 110 'val x = 10 in val f = fun y -> x + y in val x = 100 in f x' || exit 1
  >   check "$target" 8 'val make = fun x -> fun y -> x + y in val add = make 5 in add 3' || exit 1
  >   check "$target" 42 'val twice = fun f -> fun x -> f (f x) in twice (fun n -> n + 1) 40' || exit 1
  > done

FAE's named function definitions are nonrecursive sugar. Their bodies capture the surrounding environment, including an outer binding of the same function name.

  $ check fae 6 'def twice(x) = x + x in twice 3'
  $ check fae 11 'val x = 10 in def f(y) = x + y in val x = 100 in f 1'
  $ check fae 5 'val f = fun x -> x + 1 in def f(x) = f x in f 4'
  $ check fae 7 'def f(f) = f in f 7'
  $ check fae 7 '(fun ignored -> 7) (def f(x) = missing in f)'

RFAE evaluates only the selected branch. Every value other than integer zero, including closures, selects else.

  $ check rfae 7 'if0 0 then 7 else missing'
  $ check rfae 8 'if0 2 then missing else 8'
  $ check rfae 8 'if0 -1 then missing else 8'
  $ check rfae 8 'if0 (fun x -> missing) then missing else 8'
  $ check rfae 8 'def f(x) = missing in if0 f then missing else 8'
  $ check rfae 9 '(if0 0 then fun x -> x + 1 else 0) 8'
  $ check rfae 2 'if0 (if0 0 then 1 else 0) then 1 else if0 0 then 2 else 3'

Recursive closures retain self-bindings and lexical environments after escaping their definitions, being aliased, or being passed to other functions.

  $ check rfae 55 'def sum(x) = if0 x then 0 else x + sum (x - 1) in sum 10'
  $ check rfae 55 '(def sum(x) = if0 x then 0 else x + sum (x - 1) in sum) 10'
  $ check rfae 55 'def sum(x) = if0 x then 0 else x + sum (x - 1) in val f = sum in val sum = 0 in f 10'
  $ check rfae 55 'def sum(x) = if0 x then 0 else x + sum (x - 1) in (fun f -> f 10) sum'
  $ check rfae 13 'val offset = 10 in def sum(x) = if0 x then offset else x + sum (x - 1) in val offset = 100 in sum 2'
  $ check rfae 7 'def f(f) = f in f 7'
  $ check rfae 4 'def f(x) = if0 x then 1 else f (x - 1) in (def f(x) = if0 x then 2 else f (x - 1) in f 2) + f 2 + f 2'
  $ check rfae 4 'def f(x) = if0 x then fun y -> f y else fun y -> x + y in val g = f 0 in g 2 2'
  $ check rfae 3 'def f(x) = if0 x then 0 else (def g(y) = if0 y then f (x - 1) else g (y - 1) in g 2) + 1 in f 3'
  $ check rfae 7 'def f(x) = x in val f = 7 in (fun y -> f) 0'

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

Desugared bindings remain nonrecursive and eager. FAE's named functions cannot refer to themselves without a surrounding binding.

  $ rejects fae 'def f(x) = f x in f 1'
  $ for target in fae rfae; do
  >   rejects "$target" 'val f = fun x -> f x in f 1' || exit 1
  >   rejects "$target" 'val x = missing in 0' || exit 1
  >   rejects "$target" '(fun x -> 0) missing' || exit 1
  >   rejects "$target" 'val f = fun x -> x + y in val y = 1 in f 2' || exit 1
  >   rejects "$target" '1 2' || exit 1
  >   rejects "$target" '(fun x -> x) + 1' || exit 1
  > done

Recursive definitions bind only the function name in their continuation, evaluate arguments eagerly, and use captured bindings in their bodies. Closures cannot be arithmetic operands.

  $ rejects rfae 'def f(x) = x in x'
  $ rejects rfae 'def f(x) = 0 in f missing'
  $ rejects rfae 'def f(x) = x + y in val y = 1 in f 2'
  $ rejects rfae 'def f(x) = g x in def g(x) = x in f 1'
  $ rejects rfae 'def f(x) = x in f + 1'
  $ rejects rfae 'def f(x) = x in 1 - f'
  $ rejects rfae 'if0 missing then 1 else 2'
  $ rejects rfae 'if0 0 then missing else 1'
  $ rejects rfae 'if0 1 then 1 else missing'
