Function evaluation has the same results in IL, SL, and PL.

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

F1VAE supports programs with or without definitions, nested calls, forward references, and separate namespaces for functions and variables.

  $ check f1vae -3 '(2 - 8) + 3'
  $ check f1vae 16 'def twice(x) = x + x; twice(3) + twice(5)'
  $ check f1vae 12 'def twice(x) = x + x; twice(twice(3))'
  $ check f1vae 7 'def f(x) = g(x) + 1; def g(y) = y + y; f(3)'
  $ check f1vae 22 'def f(x) = val y = 2 in x + y; val x = 10 in f(x) + x'
  $ check f1vae 6 'def f(x) = x + x; val f = 3 in f(f)'
  $ check f1vae 7 'def id(x) = x; val x = 7 in id(x)'
  $ check f1vae 8 'def f(x) = x; def bad(z) = y; f(8)'

FVAE captures environments, returns functions, accepts functions as arguments, and applies parameters over captured bindings.

  $ check fvae -3 '(2 - 8) + 3'
  $ check fvae -3 '(fun x -> x) (-3)'
  $ check fvae 3 '(fun x -> fun y -> x + y) 1 2'
  $ check fvae 7 '(fun x -> x + x) 3 + 1'
  $ check fvae 42 'val twice = fun f -> fun x -> f (f x) in twice (fun n -> n + 1) 40'
  $ check fvae 11 'val x = 10 in val f = fun y -> x + y in val x = 100 in f 1'
  $ check fvae 110 'val x = 10 in val f = fun y -> x + y in val x = 100 in f x'
  $ check fvae 8 'val make = fun x -> fun y -> x + y in val add = make 5 in add 3'
  $ check fvae 4 'val x = 9 in (fun x -> x) 4'
  $ check fvae 6 'val id = fun x -> x in val f = id in f 6'
  $ check fvae 7 'val f = 8 in f-1'
  $ check fvae -2 'val f = fun x -> x + x in f(-1)'

Creating a closure does not evaluate its body.

  $ check fvae 7 '(fun f -> 7) (fun x -> missing)'

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

F1VAE requires defined function names, isolates callee variables from callers, evaluates arguments eagerly, and rejects duplicate definitions.

  $ rejects f1vae 'missing(1)'
  $ rejects f1vae 'def f(x) = x; f'
  $ rejects f1vae 'def f(x) = x + y; val y = 2 in f(1)'
  $ rejects f1vae 'def f(x) = g(0); def g(y) = x + y; f(3)'
  $ rejects f1vae 'def f(x) = 0; f(missing)'
  $ rejects f1vae 'def f(x) = x; def f(y) = y + 1; 0'

FVAE requires closures in function position and integers in arithmetic, uses the captured environment for the body, and evaluates arguments eagerly.

  $ rejects fvae 'x'
  $ rejects fvae '1 2'
  $ rejects fvae '(fun x -> x) + 1'
  $ rejects fvae '1 + (fun x -> x)'
  $ rejects fvae '(fun x -> x) - 1'
  $ rejects fvae '1 - (fun x -> x)'
  $ rejects fvae 'val f = fun x -> x + y in val y = 1 in f 2'
  $ rejects fvae '(fun x -> 0) missing'
  $ rejects fvae 'val f = fun x -> f x in f 1'
