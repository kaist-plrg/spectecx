The TVFAE evaluator produces the same results in IL, SL, and PL. The expected type and value are checked independently, and every input round-trips.

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

TVFAE constructors are first-class functions. Both branch orders use the declared payload types, and pattern variables have lexical scope.

  $ check tvfae num 8 'type Choice = Left@num + Right@num in Left 7 match { Left(x) -> x + 1; Right(y) -> y - 1 }'
  $ check tvfae num 8 'type Choice = Left@num + Right@num in Right 7 match { Left(x) -> x - 1; Right(y) -> y + 1 }'
  $ check tvfae num 8 'type Choice = Left@num + Right@(num -> num) in Right (fun (x : num) -> x + 1) match { Right(f) -> f 7; Left(n) -> n }'
  $ check tvfae num 7 'type Choice = Left@num + Right@(num -> num) in Left 7 match { Right(f) -> f 1; Left(n) -> n }'
  $ check tvfae num 3 'type T = A@num + B@num in (fun (make : num -> T) -> make 3 match { A(x) -> x; B(y) -> y }) A'
  $ check tvfae num 7 'type T = A@num + B@num in val make = A in make 7 match { A(x) -> x; B(y) -> y }'
  $ check tvfae num 13 'type T = A@num + B@num in val A = fun (x : num) -> x + 10 in A 3'
  $ check tvfae num 10 'type T = A@num + B@num in val x = 7 in (A 3 match { A(x) -> x; B(y) -> y }) + x'
  $ check tvfae num 4 'type T = A@num + B@num in A 3 match { A(B) -> B + 1; B(A) -> A }'
  $ check tvfae num 3 'type T = T@num + Other@num in T 3 match { T(x) -> x; Other(y) -> y }'
  $ check tvfae num 4 'type T = A@num + B@num in def extract(v : T) = v match { A(x) -> x; B(y) -> y } in extract (B 4)'
  $ check tvfae num 11 'type T = A@num + B@num in val x = 10 in val f = fun (v : T) -> v match { A(y) -> x + y; B(y) -> x - y } in val x = 100 in f (A 1)'

TVFAE follows the Scala implementation for repeated constructor names. The later declaration supplies the payload type, and the first matching case is selected.

  $ check tvfae num 0 'type T = A@num + A@num in 0'
  $ check tvfae num 1 'type T = A@num + A@num in A 5 match { A(x) -> 1; A(y) -> 2 }'
  $ check tvfae num 6 'type T = A@num + A@(num -> num) in A (fun (x : num) -> x + 1) match { A(f) -> f 5; A(g) -> g 6 }'
  $ check tvfae num 6 'type T = A@(num -> num) + A@num in A 5 match { A(x) -> x + 1; A(y) -> y + 2 }'
  $ check tvfae num 3 'type T = A@num + B@num in A 3 match { A(x) -> x; A(y) -> y + 10 }'

Recursive declarations make their own type name available to both payload annotations. Local type names may appear in results under the book's Typ-TypeDef rule.

  $ check tvfae num 0 'type Nat = Zero@num + Succ@Nat in 0'
  $ check tvfae num 2 'type Nat = Zero@num + Succ@Nat in Succ (Succ (Zero 0)) match { Zero(z) -> z; Succ(n) -> n match { Succ(m) -> 2; Zero(z) -> 1 } }'
  $ check tvfae num 3 'type Tree = Leaf@num + Branch@(Tree -> Tree) in Leaf 3 match { Leaf(n) -> n; Branch(f) -> 0 }'
  $ check tvfae num 3 'type Outer = A@num + B@num in type Inner = C@Outer + D@num in C (A 3) match { C(v) -> v match { A(x) -> x; B(y) -> y }; D(z) -> z }'
  $ check tvfae T 'A (3)' 'type T = A@num + B@num in A 3'
  $ check_type tvfae '(num -> T)' 'type T = A@num + B@num in A'
  $ check tvfae T 'A (3)' 'val make = (type T = A@num + B@num in fun (x : num) -> A x) in make 3'
  $ check tvfae num 4 'type T = A@num + B@num in type T = C@num + D@num in C 4 match { C(x) -> x; D(y) -> y }'

TVFAE retains TFAE's arithmetic, lexical bindings, and nonrecursive local function definitions.

  $ for target in tvfae; do
  >   check "$target" num -3 '(fun (x : num) -> x + 1) (-4)' || exit 1
  >   check "$target" num 21 'val x = 10 in (val x = x + 1 in x) + x' || exit 1
  >   check "$target" num 6 'def twice(x : num) = x + x in twice 3' || exit 1
  > done

Ill-formed annotations and invalid constructors or applications must fail after parsing succeeds.

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

  $ rejects tvfae 'fun (x : Missing) -> x'
  $ rejects tvfae 'fun (x : num -> Missing) -> 0'
  $ rejects tvfae 'val T = 3 in fun (x : T) -> x'
  $ rejects tvfae 'type T = A@Missing + B@num in 0'
  $ rejects tvfae 'type T = A@num + B@(Missing -> num) in 0'
  $ rejects tvfae 'type T = A@Missing + A@num in 0'
  $ rejects tvfae 'type T = A@(num -> num) + A@num in A (fun (x : num) -> x)'
  $ rejects tvfae 'type T = A@num + B@num in A (fun (x : num) -> x)'
  $ rejects tvfae 'type T = A@num + B@num in A + 1'
  $ rejects tvfae 'type T = A@num + B@num in (A 3) 4'
  $ rejects tvfae 'type T = A@num + B@num in 3 match { A(x) -> x; B(y) -> y }'
  $ rejects tvfae 'type T = A@num + B@num in A match { A(x) -> x; B(y) -> y }'
  $ rejects tvfae 'type T = A@num + B@num in A 3 match { A(x) -> x; C(y) -> y }'
  $ rejects tvfae 'type T = A@num + B@num in A 3 match { A(x) -> x; B(y) -> fun (z : num) -> z }'
  $ rejects tvfae 'type T = A@num + B@num in A 3 match { A(x) -> x; B(y) -> x }'
  $ rejects tvfae 'type T = A@num + B@num in A 3 match { A(x) -> x; B(y) -> missing }'
  $ rejects tvfae 'val v = (type T = A@num + B@num in A 3) in v match { A(x) -> x; B(y) -> y }'
  $ rejects tvfae 'type T = A@num + B@num in type U = C@num + D@num in (fun (x : T) -> 0) (C 1)'
  $ rejects tvfae 'type T = A@num + B@num in type U = C@num + D@num in C 1 match { A(x) -> x; B(y) -> y }'

The book's named type rules admit this counterexample. Type checking succeeds, but evaluation fails. An unselected match branch is not evaluated.

  $ runtime_error() {
  >   target=$1
  >   expression=$2
  >   printf '%s\n' "$expression" > "input.$target"
  >   spectec "$target" parse -r -p "input.$target" --color never > /dev/null || return 1
  >   for mode in '' --sl --pl; do
  >     actual=$(spectec "$target" typecheck $mode -p "input.$target" --color never) || return 1
  >     test "$actual" = num || return 1
  >     if spectec "$target" eval $mode -p "input.$target" --color never > error 2>&1; then
  >       printf 'unexpected evaluation success: %s\n' "$expression"
  >       return 1
  >     fi
  >     grep -Eq 'source: (il|sl|pl)-interp' error || { cat error; return 1; }
  >   done
  > }
  $ tvfae_bad='type T = A@num + B@num in (fun (f : T -> num) -> type T = C@num + D@num in f (C 1)) (fun (v : T) -> v match { A(x) -> x; B(y) -> y })'
  $ runtime_error tvfae "$tvfae_bad"
  $ check tvfae num 7 "type Choice = Left@num + Right@num in Left 7 match { Left(x) -> x; Right(y) -> ($tvfae_bad) }"
  $ check tvfae num 7 "type Choice = Left@num + Right@num in Right 7 match { Left(x) -> ($tvfae_bad); Right(y) -> y }"

Repeated match cases may leave a variant unhandled. Evaluation cannot try a later case when the first matching case fails.

  $ runtime_error tvfae 'type T = A@num + B@num in B 3 match { A(x) -> x; A(y) -> y }'
  $ runtime_error tvfae "type T = A@num + A@num in A 3 match { A(x) -> ($tvfae_bad); A(y) -> y }"

Batch distinguishes static errors from the documented runtime errors for both tasks.

  $ mkdir -p corpus/nested
  $ for target in tvfae; do
  >   printf '%s\n' '(fun (x : num) -> x + 1) 2' > "corpus/nested/good.$target"
  >   printf '%s\n' 'fun (x : Missing) -> x' > "corpus/static_error.$target"
  >   bad=$tvfae_bad
  >   printf '%s\n' "$bad" > "corpus/reuse_runtime_error.$target"
  >   for mode in '' --sl --pl; do
  >     spectec "$target" batch $mode --batch-dir corpus --color never > summary || exit 1
  >     grep -Fx 'typechecker: 3/3 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >     grep -Fx 'evaluator: 3/3 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >   done
  > done
