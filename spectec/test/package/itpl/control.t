Lazy evaluation and continuations have the same results in IL, SL, and PL.

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

LFAE leaves unused arguments unevaluated, including erroneous expressions. Nonrecursive val bindings inherit lazy application semantics.

  $ check lfae 7 '(fun x -> 7) missing'
  $ check lfae 7 'val x = missing in 7'
  $ check lfae 7 '(fun x -> 7) (1 2)'
  $ check lfae 3 '(fun x -> fun y -> y) missing 3'
  $ check lfae 7 'def unused(x) = missing in 7'

Delayed expressions retain the caller's environment. Strict evaluation follows chains of delayed values in arithmetic, function position, and the final result.

  $ check lfae 2 '(fun x -> x) (1 + 1)'
  $ check lfae 3 'val id = fun x -> x in id (id (id 3))'
  $ check lfae 6 'val x = 3 in val y = x in val z = y in z + z'
  $ check lfae 0 'val x = 3 in val y = x in y - x'
  $ check lfae 4 '(fun f -> f 3) ((fun x -> x) (fun y -> y + 1))'
  $ check lfae 10 'val x = 10 in val f = fun y -> (fun x -> y) 20 in f x'
  $ check lfae 1 'val x = 1 in (fun x -> x) x'
  $ check lfae 110 'val x = 10 in val f = fun y -> x + y in val x = 100 in f x'
  $ check lfae 3 'val x = missing in val x = 3 in x'

LFAE and KFAE agree on terminating arithmetic and function examples.

  $ for target in lfae kfae; do
  >   check "$target" -3 '(2 - 8) + 3' || exit 1
  >   check "$target" -3 '(fun x -> x) (-3)' || exit 1
  >   check "$target" 21 'val x = 10 in (val x = x + 1 in x) + x' || exit 1
  >   check "$target" 11 'val x = 10 in val f = fun y -> x + y in val x = 100 in f 1' || exit 1
  >   check "$target" 42 'val twice = fun f -> fun x -> f (f x) in twice (fun n -> n + 1) 40' || exit 1
  >   check "$target" 6 'def twice(x) = x + x in twice 3' || exit 1
  >   check "$target" 5 'val f = fun x -> x + 1 in def f(x) = f x in f 4' || exit 1
  > done

The chapter 16 examples distinguish applying a continuation from returning from an ordinary function.

  $ check kfae 3 '1 + (vcc x in (x 2) + 3)'
  $ check kfae 7 '1 + (val x = fun y -> 1 + y in (x 2) + 3)'
  $ check kfae 4 'vcc x in (vcc y in x (1 + (vcc z in y z))) 3'

Capturing binds the continuation of the entire vcc expression. Calls discard intervening computation and evaluate their argument before resuming the saved continuation.

  $ check kfae 7 'vcc k in 7'
  $ check kfae 7 'vcc k in k 7'
  $ check kfae 7 'vcc k in k 7 + missing'
  $ check kfae 3 'vcc k in k (1 + 2)'
  $ check kfae 6 '1 + (vcc k in k (2 + 3))'
  $ check kfae 7 '10 - (vcc k in k 3 + 100)'
  $ check kfae 5 '(vcc k in k 8) - 3'
  $ check kfae 7 'vcc out in 1 + (vcc inner in out 7)'
  $ check kfae 4 '1 + (vcc out in 2 + (vcc inner in out 3))'
  $ check kfae 7 'vcc out in (fun f -> 1 + f 7) out'

Saved arithmetic and application frames restore their environments. Continuations may escape their vcc scope and be invoked more than once.

  $ check kfae 11 'val x = 10 in (vcc k in (fun x -> k 1) 100) + x'
  $ check kfae 10 'val x = 10 in (vcc k in (fun x -> k (fun y -> y)) 100) x'
  $ check kfae 7 'val f = vcc k in fun ignored -> k (fun x -> x) in f 7'
  $ check kfae 7 'val f = vcc k in k (fun ignored -> k (fun x -> x)) in f 7'
  $ check kfae 3 '((vcc k in k) (fun x -> x)) 3'
  $ check kfae 3 'vcc k in (fun k -> k 2) (fun x -> x + 1)'

Both operands are evaluated before arithmetic or application checks their values. A right operand can escape an otherwise invalid pending operation.

  $ check kfae 7 'vcc out in (fun x -> x) + (out 7)'
  $ check kfae 7 'vcc out in (fun x -> x) - (out 7)'
  $ check kfae 7 'vcc out in 1 (out 7)'
  $ check kfae 7 'vcc out in (out 7) + missing'
  $ check kfae 7 'vcc out in (out 7) missing'

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

LFAE forces required arguments, including the whole program's result, and resolves free identifiers in the environment captured with the delayed expression.

  $ rejects lfae '(fun x -> x) missing'
  $ rejects lfae '(fun x -> x + 1) missing'
  $ rejects lfae '(fun f -> f 1) missing'
  $ rejects lfae '(fun f -> f 1) 2'
  $ rejects lfae 'val x = missing in val y = x in y'
  $ rejects lfae 'val f = fun x -> (fun y -> x) 1 in f y'
  $ rejects lfae '(fun x -> x) (1 2)'

KFAE evaluates arguments eagerly and requires valid values when the pending operation is reached. A continuation cannot be used as an arithmetic operand.

  $ rejects kfae '(fun x -> 0) missing'
  $ rejects kfae 'val x = missing in 0'
  $ rejects kfae 'vcc k in k missing'
  $ rejects kfae 'vcc out in missing + out 7'
  $ rejects kfae 'vcc out in missing (out 7)'
  $ rejects kfae 'vcc k in k + 1'
  $ rejects kfae 'vcc k in 1 - k'
  $ rejects kfae '1 + (vcc k in k k)'

Both languages preserve lexical scope, reject invalid arithmetic and applications, and keep local def nonrecursive.

  $ for target in lfae kfae; do
  >   rejects "$target" 'missing' || exit 1
  >   rejects "$target" '1 2' || exit 1
  >   rejects "$target" '(fun x -> x) + 1' || exit 1
  >   rejects "$target" '1 - (fun x -> x)' || exit 1
  >   rejects "$target" 'def f(x) = f x in f 1' || exit 1
  >   rejects "$target" 'val f = fun x -> x + y in val y = 1 in f 2' || exit 1
  > done
