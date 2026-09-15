STFAE subtyping produces the expected types and values in IL, SL, and PL. Each input also round-trips.

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

Records support width, depth, permutation, empty records, and function fields. Projection binds tighter than application.

  $ check stfae '{}' '{}' '{}'
  $ check stfae num 7 '(fun (r : {a : num}) -> r.a) {b = 2, a = 7}'
  $ check stfae num 8 '(fun (r : {inner : {a : num}}) -> r.inner.a) {inner = {b = 2, a = 8}, extra = 0}'
  $ check stfae num 3 '(fun (f : {a : num, b : num} -> {result : num}) -> (f {a = 3, b = 4}).result) (fun (r : {a : num}) -> {extra = 0, result = r.a})'
  $ check stfae num 11 'val x = 10 in val r = {f = fun (n : num) -> x + n} in val x = 100 in r.f 1'
  $ check stfae num 7 '(fun (r : {a : num, b : num}) -> r.a + r.b) {b = 3, a = 4}'
  $ check stfae num 2 '{f = fun (x : num) -> x + 1}.f 1'
  $ check stfae num 3 '((fun (r : {a : num}) -> r) {a = 3, b = 4}).a'
  $ check stfae num 3 'def get(r : {a : num}) = r.a in get {b = 1, a = 3}'
  $ check stfae num 3 'val x = 1 in {a = val x = 2 in x, b = x}.a + x'
  $ check stfae num 10 '{val = 1, in = 2, fun = 3, num = 4}.val + {in = 2}.in + {fun = 3}.fun + {num = 4}.num'
  $ check stfae num 7 '(fun (x : top) -> 7) 1'
  $ check stfae num 7 '(fun (x : top) -> 7) (fun (x : num) -> x)'
  $ check stfae num 7 '(fun (x : top) -> 7) {}'
  $ check stfae num 7 '(fun (x : {}) -> 7) {a = 1}'
  $ check stfae top 3 '(fun (x : top) -> x) 3'
  $ check stfae num 3 '(fun (f : num -> top) -> 3) (fun (x : top) -> 1)'
  $ check stfae num 3 '(fun (f : num -> num) -> f 0) (fun (x : top) -> 3)'

  $ check stfae num -3 '{a = -4}.a - (-1)'

Minimal types retain record field order. Bottom-typed expressions can be used as numbers, functions, or records.

  $ type_is() {
  >   target=$1
  >   expected=$2
  >   expression=$3
  >   printf '%s\n' "$expression" > "input.$target"
  >   spectec "$target" parse -r -p "input.$target" --color never > /dev/null || return 1
  >   for mode in '' --sl --pl; do
  >     actual=$(spectec "$target" typecheck $mode -p "input.$target" --color never) || return 1
  >     if [ "$actual" != "$expected" ]; then
  >       printf '%s %s: expected type %s, got %s\n' "$target" "$mode" "$expected" "$actual"
  >       return 1
  >     fi
  >   done
  > }

  $ type_is stfae '{}' '{}'
  $ type_is stfae '{b : num, a : (num -> num)}' '{b = 2, a = fun (x : num) -> x}'
  $ type_is stfae '(bottom -> bottom)' 'fun (x : bottom) -> x'
  $ type_is stfae '(bottom -> bottom)' 'fun (x : bottom) -> x.a'
  $ type_is stfae '(bottom -> bottom)' 'fun (x : bottom) -> x 1'
  $ type_is stfae '(bottom -> num)' 'fun (x : bottom) -> x + 1'
  $ type_is stfae '(bottom -> num)' 'fun (x : bottom) -> (fun (y : num) -> y) x'
  $ type_is stfae '(bottom -> bottom)' 'fun (x : bottom) -> x.missing 1'
  $ type_is stfae '({a : bottom} -> bottom)' 'fun (r : {a : bottom}) -> r.a'
  $ type_is stfae '(top -> top)' 'fun (x : top) -> x'


  $ type_is stfae '(bottom -> num)' 'fun (x : bottom) -> 1 - x'

Invalid operations, duplicate labels, and incompatible types fail both commands after parsing succeeds.

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


  $ rejects stfae missing
  $ rejects stfae '{a = 1}.b'
  $ rejects stfae '{a = 1, a = 2}'
  $ rejects stfae 'fun (r : {a : num, a : top}) -> 0'
  $ rejects stfae '(fun (r : {a : num, b : num}) -> r.b) {a = 1}'
  $ rejects stfae '(fun (f : {a : num} -> num) -> f {a = 1}) (fun (r : {a : num, b : num}) -> r.b)'
  $ rejects stfae '(fun (r : {a : num}) -> r) {a = 3, b = 4}.a'
  $ rejects stfae '(fun (x : {}) -> 0) 1'
  $ rejects stfae 'fun (x : top) -> x + 1'
  $ rejects stfae 'fun (x : top) -> x 1'
  $ rejects stfae 'fun (x : top) -> x.a'
  $ rejects stfae 'fun (x : bottom) -> x missing'
  $ rejects stfae '(fun (f : top -> num) -> 0) (fun (x : num) -> x)'
  $ rejects stfae '(fun (f : num -> num) -> 0) (fun (x : num) -> (fun (y : top) -> y) x)'
  $ rejects stfae '(fun (r : {a : {b : num}}) -> r.a.b) {a = {c = 1}}'
  $ rejects stfae '{a = 1, b = missing}.a'
  $ rejects stfae 'fun (f : num -> {a : num, a : num}) -> 0'
  $ rejects stfae 'fun (x : {a : {b : num, b : top}}) -> 0'
  $ rejects stfae '(fun (x : bottom) -> 0) 1'

  $ rejects stfae '(fun (x : num) -> x) - 1'

Batch checks both tasks across nested input directories in every interpreter.

  $ mkdir -p corpus/nested
  $ for target in stfae; do
  >   printf '%s\n' '1 + 2' > "corpus/nested/good.$target"
  >   printf '%s\n' missing > "corpus/free_error.$target"
  >   for mode in '' --sl --pl; do
  >     spectec "$target" batch $mode --batch-dir corpus --color never > summary || exit 1
  >     grep -Fx 'typechecker: 2/2 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >     grep -Fx 'evaluator: 2/2 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >   done
  > done

Checking against an explicit supertype accepts subsumption, rejects narrowing and malformed types, and direct evaluation requires all record fields even before projection.

  $ sed 's/Run_prog/Run_reference/g' ../../../specs/itpl/stfae/spec.spectec > reference.spectec
  $ printf '%s\n' checks > checks.stfae
  $ for mode in '' --sl --pl; do
  >   spectec stfae eval $mode --spec reference.spectec --spec subtyping.spectec -p checks.stfae --color never || exit 1
  > done
  1
  1
  1
  $ for name in narrow duplicate strict; do
  >   printf '%s\n' "$name" > checks.stfae
  >   for mode in '' --sl --pl; do
  >     if spectec stfae eval $mode --spec reference.spectec --spec subtyping.spectec -p checks.stfae --color never > error 2>&1; then
  >       printf 'unexpected success: %s\n' "$name"
  >       exit 1
  >     fi
  >     grep -Eq 'source: (il|sl|pl)-interp' error || { cat error; exit 1; }
  >   done
  > done

The example corpora include function results and static failures.

  $ for target in stfae; do
  >   for mode in '' --sl --pl; do
  >     spectec "$target" batch $mode --batch-dir "../../../testdata/interp/itpl/$target" --color never > summary || { cat summary; exit 1; }
  >     grep -Fx 'typechecker: 12/12 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >     grep -Fx 'evaluator: 12/12 passed, 0 failed' summary > /dev/null || { cat summary; exit 1; }
  >   done
  > done
