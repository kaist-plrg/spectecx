Typed targets infer the same types and produce the same values in IL, SL, and PL. Successful inputs round-trip with their annotations intact.

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

Both languages check numeric operations, lexical scope, binding initializers, and higher-order applications. Arrow types associate to the right, and parentheses can select a function parameter type.

  $ for target in tfae trfae; do
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

TRFAE checks both branches but evaluates only the selected branch. Recursive functions can return functions, escape their definition, and capture outer bindings. A parameter shadows a self-binding with the same name.

  $ check trfae num 55 'def sum(x : num) : num = if0 x then 0 else x + sum (x - 1) in sum 10'
  $ check trfae num 1 'if0 0 then 1 else 2'
  $ check trfae num 2 'if0 (-1) then 1 else 2'
  $ check trfae num 3 'if0 0 then if0 1 then 2 else 3 else 4'
  $ check trfae num 7 'def loop(x : num) : num = loop x in if0 0 then 7 else loop 0'
  $ check trfae num 7 'def loop(x : num) : num = loop x in if0 1 then loop 0 else 7'
  $ check trfae num 5 '(if0 0 then fun (x : num) -> x + 1 else fun (x : num) -> x - 1) 4'
  $ check trfae num 4 'def f(f : num) : num = f + 1 in f 3'
  $ check trfae num 4 'def f(f : num -> num) : num = f 3 in f (fun (x : num) -> x + 1)'
  $ check trfae num 13 'val offset = 10 in val f = def sum(x : num) : num = if0 x then offset else 1 + sum (x - 1) in sum in val offset = 100 in f 3'
  $ check trfae num 10 'def f(x : num) : num -> num = if0 x then fun (y : num) -> y else fun (y : num) -> (f (x - 1)) (y + 1) in f 3 7'
  $ check trfae num 9 'def f(x : num) : num = if0 x then 0 else 1 + f (x - 1) in def g(x : num) : num = f x + x in g 3 + f 3'
  $ check trfae num 7 'val x = 7 in def f(x : num) : num = x in x'
  $ check_type trfae '(num -> num)' 'def f(x : num) : num = x in f'

Type checking terminates for a well-typed recursive program even when evaluation would diverge.

  $ printf '%s\n' 'def loop(x : num) : num = loop x in loop 0' > loop.trfae
  $ for mode in '' --sl --pl; do
  >   spectec trfae typecheck $mode -p loop.trfae --color never || exit 1
  > done
  num
  num
  num

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

  $ for target in tfae trfae; do
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

TRFAE requires a numeric condition, equal branch types, and the declared recursive return type. Function parameters are confined to the function body.

  $ rejects trfae 'if0 (fun (x : num) -> x) then 1 else 2'
  $ rejects trfae 'if0 0 then 1 else fun (x : num) -> x'
  $ rejects trfae 'if0 1 then fun (x : num) -> x else 2'
  $ rejects trfae 'if0 0 then 1 else missing'
  $ rejects trfae 'def f(x : num) : num = fun (y : num) -> y in 7'
  $ rejects trfae 'def f(x : num) : num -> num = x in 7'
  $ rejects trfae 'def f(x : num) : num = f (fun (y : num) -> y) in 7'
  $ rejects trfae 'def f(x : num) : num = missing in 7'
  $ rejects trfae 'def f(x : num) : num = x in x'
  $ rejects trfae '(def f(x : num) : num = x in f 1) + f 1'
  $ rejects trfae 'def f(f : num) : num = f 3 in 7'
  $ rejects trfae 'val f = fun (x : num) -> x in def f(x : num) : num = f in 7'

Batch runs both tasks, collects inputs recursively, and treats _error files as expected failures.

  $ mkdir -p corpus/nested
  $ for target in tfae trfae; do
  >   printf '%s\n' '(fun (x : num) -> x + 1) 2' > "corpus/nested/good.$target"
  >   printf '%s\n' 'fun (x : num) -> missing' > "corpus/unused_error.$target"
  >   for mode in '' --sl --pl; do
  >     spectec "$target" batch $mode --batch-dir corpus --color never > summary || exit 1
  >     grep -Fx 'typechecker: 2/2 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >     grep -Fx 'evaluator: 2/2 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >   done
  > done
