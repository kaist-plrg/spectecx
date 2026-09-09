TFAE infers the same types and produces the same values in IL, SL, and PL. Successful inputs round-trip with their annotations intact.

  $ check() {
  >   target=$1
  >   expected_type=$2
  >   expected_value=$3
  >   expression=$4
  >   printf '%s\n' "$expression" > "input.$target"
  >   for mode in '' --sl --pl; do
  >     actual=$(spectec "$target" typecheck $mode -p "input.$target" --color never) || return 1
  >     if [ "$actual" != "$expected_type" ]; then
  >       printf '%s %s: expected type %s, got %s\n' "$target" "$mode" "$expected_type" "$actual"
  >       return 1
  >     fi
  >     actual=$(spectec "$target" eval $mode -p "input.$target" --color never) || return 1
  >     if [ "$actual" != "$expected_value" ]; then
  >       printf '%s %s: expected value %s, got %s\n' "$target" "$mode" "$expected_value" "$actual"
  >       return 1
  >     fi
  >   done
  >   spectec "$target" parse -r -p "input.$target" --color never > /dev/null
  > }

  $ check_type() {
  >   target=$1
  >   expected=$2
  >   printf '%s\n' "$3" > "input.$target"
  >   for mode in '' --sl --pl; do
  >     actual=$(spectec "$target" typecheck $mode -p "input.$target" --color never) || return 1
  >     test "$actual" = "$expected" || { printf 'expected %s, got %s\n' "$expected" "$actual"; return 1; }
  >   done
  >   spectec "$target" parse -r -p "input.$target" --color never > /dev/null
  > }

TFAE checks numeric operations, lexical scope, binding initializers, and higher-order applications. Arrow types associate to the right, and parentheses can select a function parameter type.

  $ for target in tfae; do
  >   check "$target" num -3 '(2 - 8) + 3' || exit 1
  >   check "$target" num -3 '(fun (x : num) -> x) (-3)' || exit 1
  >   check "$target" num 21 'val x = 10 in (val x = x + 1 in x) + x' || exit 1
  >   check "$target" num 11 'val x = 10 in val f = fun (y : num) -> x + y in val x = 100 in f 1' || exit 1
  >   check "$target" num 3 'val x = 1 in (fun (x : num) -> x + 1) (x + 1)' || exit 1
  >   check "$target" num 42 'val twice = fun (f : num -> num) -> fun (x : num) -> f (f x) in twice (fun (n : num) -> n + 1) 40' || exit 1
  >   check "$target" num 7 '(fun (f : num -> num -> num) -> f 3 4) (fun (x : num) -> fun (y : num) -> x + y)' || exit 1
  >   check "$target" num 5 '(fun (f : (num -> num) -> num) -> f (fun (x : num) -> x + 1)) (fun (g : num -> num) -> g 4)' || exit 1
  >   check "$target" num 5 'val f = fun (x : num) -> x + 1 in val f = f 4 in f' || exit 1
  >   check "$target" num 3 '(fun (x : num -> num) -> (fun (x : num) -> x) 3) (fun (y : num) -> y)' || exit 1
  >   check "$target" num 7 'val x = 4 in val f = (fun (x : num) -> fun (y : num) -> x + y) 3 in f x' || exit 1
  >   check_type "$target" '(num -> num)' 'fun (x : num) -> x' || exit 1
  >   check_type "$target" '((num -> num) -> (num -> num))' 'fun (f : num -> num) -> f' || exit 1
  >   check_type "$target" '((num -> (num -> num)) -> (num -> (num -> num)))' 'fun (f : num -> num -> num) -> f' || exit 1
  >   check_type "$target" '(((num -> num) -> num) -> ((num -> num) -> num))' 'fun (f : (num -> num) -> num) -> f' || exit 1
  >   check "$target" num 3 'val num = 2 in (fun (num : num) -> num + 1) num' || exit 1
  > done

TFAE local definitions are nonrecursive sugar for val and an annotated function. Function bodies use the environment outside the definition.

  $ check tfae num 6 'def twice(x : num) = x + x in twice 3'
  $ check tfae num 5 'val f = fun (x : num) -> x + 1 in def f(x : num) = f x in f 4'
  $ check tfae num 3 'def if0(then : num) = then + 1 in val else = 2 in if0 else'

Ill-typed expressions parse successfully but fail both typecheck and eval, including errors in unused function bodies and bindings.

  $ rejects() {
  >   target=$1
  >   expression=$2
  >   printf '%s\n' "$expression" > "input.$target"
  >   spectec "$target" parse -r -p "input.$target" --color never > /dev/null || return 1
  >   for command in typecheck eval; do
  >     for mode in '' --sl --pl; do
  >       if spectec "$target" "$command" $mode -p "input.$target" --color never > error 2>&1; then
  >         printf 'unexpected %s success: %s\n' "$command" "$expression"
  >         return 1
  >       fi
  >       grep -Eq 'source: (il|sl|pl)-interp' error || { cat error; return 1; }
  >     done
  >   done
  > }

  $ for target in tfae; do
  >   rejects "$target" 'missing' || exit 1
  >   rejects "$target" '1 2' || exit 1
  >   rejects "$target" '(fun (x : num) -> x) + 1' || exit 1
  >   rejects "$target" '1 - (fun (x : num) -> x)' || exit 1
  >   rejects "$target" '(fun (x : num) -> 7) (fun (y : num) -> y)' || exit 1
  >   rejects "$target" '(fun (f : num -> num) -> 7) 3' || exit 1
  >   rejects "$target" '(fun (f : num -> num) -> f 3) (fun (x : num) -> fun (y : num) -> y)' || exit 1
  >   rejects "$target" '(fun (f : (num -> num) -> num) -> 7) (fun (x : num) -> fun (y : num) -> y)' || exit 1
  >   rejects "$target" 'fun (x : num) -> missing' || exit 1
  >   rejects "$target" 'fun (x : num) -> x x' || exit 1
  >   rejects "$target" 'val x = missing in 7' || exit 1
  >   rejects "$target" 'val f = fun (x : num) -> x + missing in 7' || exit 1
  >   rejects "$target" 'val x = x in x' || exit 1
  >   rejects "$target" 'val f = fun (x : num) -> x + y in val y = 1 in f 2' || exit 1
  >   rejects "$target" '(val x = 1 in x) + x' || exit 1
  > done
  $ rejects tfae 'def f(x : num) = f x in 7'
  $ rejects tfae 'val f = fun (x : num) -> f x in 7'

Batch runs both tasks, collects inputs recursively, and treats _error files as expected failures.

  $ mkdir -p corpus/nested
  $ for target in tfae; do
  >   printf '%s\n' '(fun (x : num) -> x + 1) 2' > "corpus/nested/good.$target"
  >   printf '%s\n' 'fun (x : num) -> missing' > "corpus/unused_error.$target"
  >   for mode in '' --sl --pl; do
  >     spectec "$target" batch $mode --batch-dir corpus --color never > summary || exit 1
  >     grep -Fx 'typechecker: 2/2 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >     grep -Fx 'evaluator: 2/2 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >   done
  > done
